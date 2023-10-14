package kinasr.nsr_shot.shot_manager;

import kinasr.nsr_shot.cv_proccess.CVManager;
import kinasr.nsr_shot.model.*;
import kinasr.nsr_shot.utility.Helper;
import kinasr.nsr_shot.utility.config.ConfigHandler;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

import static kinasr.nsr_shot.utility.Constant.REF_IMAGE_STAMP;
import static kinasr.nsr_shot.utility.Helper.saveShot;

public class ShotExecutor {
    private static final Logger logger = LoggerFactory.getLogger(ShotExecutor.class);

    private final WebDriver driver;
    private final WebElement element;
    private final ScreenshotModel ref;
    private final ScreenshotModel shot;
    private final ShotOption option;
    private List<TechniqueRecord> techniques = new ArrayList<>();
    private ShotResult result;
    private StringBuilder msg;

    public ShotExecutor(WebDriver driver,
                        WebElement element,
                        ScreenshotModel ref,
                        ScreenshotModel shot,
                        ShotOption option) {
        this.driver = driver;
        this.element = element;
        this.ref = ref;
        this.shot = shot;
        this.option = option;
    }

    /**
     * Adds a new technique record to the list of techniques.
     *
     * @param technique the similarity technique to be added
     * @param threshold the threshold value for the technique
     * @param operation the operation to be performed
     * @return the current ShotExecutor instance
     */
    public ShotExecutor technique(SimilarityTechniques technique, Double threshold, Operation operation) {
        techniques.add(new TechniqueRecord(
                technique,
                threshold,
                operation
        ));

        return this;
    }

    /**
     * Performs a shot and returns the result.
     *
     * @return The shot result.
     */
    public ShotResult perform() {
        result = new ShotResult(new ShotRecord(ref.image(), ref.fullPath()));

        var isMatch = Helper.await(
                ConfigHandler.retakeShotCount(),
                ConfigHandler.retakeShotInterval(),
                this::isMatch
        );

        return result.isMatch(isMatch);
    }

    /**
     * Verifies the result of the perform method.
     */
    public void verify() {
        perform().verify();
    }

    /**
     * Checks if the current screenshot matches the reference image.
     *
     * @return true if the screenshot matches the reference image, false otherwise.
     */
    private boolean isMatch() {
        msg = new StringBuilder();

        var cv = screenshot();
        var isMatch = isMatch(cv);

        if (Boolean.TRUE.equals(isMatch)) {
            result.addMatchedShot(new ShotRecord(shot.image(), shot.fullPath()));
            logger.info("Assert that shot matching the reference using [\n{}", msg);
        } else {
            result.addShot(new ShotRecord(shot.image(), shot.fullPath()));
            logger.warn("Image not match the reference using [\n{}", msg);
        }
        cv.close();

        return isMatch;
    }

    /**
     * Checks if the given CV matches all the techniques and their thresholds.
     *
     * @param cv The CV to be checked.
     * @return True if the CV matches all the techniques and their thresholds, false otherwise.
     */
    private boolean isMatch(CVManager cv) {
        var isMatch = true;
        if (techniques.isEmpty())
            techniques = ConfigHandler.techniques();

        for (TechniqueRecord techniqueRecord : techniques) {
            var diff = cv.getDiff(techniqueRecord.technique());
            isMatch = isMatch && techniqueRecord.operation().operationResult(diff, techniqueRecord.threshold());
            msg.append("   <")
                    .append(techniqueRecord.technique().name())
                    .append("> technique - actual <")
                    .append(diff)
                    .append("> while it should be ")
                    .append(techniqueRecord.operation().name())
                    .append(" than expected <")
                    .append(techniqueRecord.threshold())
                    .append(">")
                    .append("\n");
        }
        msg.append("]");

        return isMatch;
    }

    /**
     * Checks the size of the images.
     * If the 'resizeImage' option is enabled, resizes the second image to match the size of the first image.
     * Otherwise, throws an AssertionError if the two images have different sizes.
     *
     * @param cv The CVManager object.
     */
    private void checkImageSize(CVManager cv) {
        try (var shotSize = cv.image1Size(); var refSize = cv.image2Size()) {
            if (Boolean.TRUE.equals(option.resizeImage()))
                cv.resizeImg2ToMatchImg1();
            else if (cv.isNotTheTwoImagesHaveTheSameSize())
                throw new AssertionError("The two images are not the same size. - Shot size <" +
                        shotSize.width() + "*" + shotSize.height() + "> - Reference size <" +
                        refSize.width() + "*" + refSize.height() + ">");
        }
    }

    /**
     * Takes a screenshot and performs image comparison.
     * Returns a CVManager object containing the reference image and the screenshot image.
     */
    private CVManager screenshot() {
        shot.image(takeShot(element));

        if (!ref.isLoaded())
            saveRefAndThrow();

        var cv = new CVManager(ref.image(), shot.image());
        checkImageSize(cv);

        if (ConfigHandler.saveShot()) {
            shot.timestamp(Helper.timestamp());
            saveShot(shot.image(), shot.path(), shot.fullName());
        }
        return cv;
    }

    /**
     * Takes a screenshot and returns the byte array representation.
     *
     * @param element The WebElement to take a screenshot of.
     * @return The byte array representation of the screenshot.
     */
    private byte[] takeShot(WebElement element) {
        return element == null ?
                ShotTaker.takeFullShot(driver) :
                ShotTaker.takeElementShot(element);
    }

    /**
     * Saves the reference image and throws an AssertionError if no reference image is found.
     */
    private void saveRefAndThrow() {
        ref.width(shot.width()).height(shot.height());
        saveShot(shot.image(), ref.path(), ref.fullName());

        Helper.repeat(ConfigHandler.multiRefCount(), ConfigHandler.multiRefInterval(),
                (count) -> {
                    var img = takeShot(element);
                    ref.timestamp(REF_IMAGE_STAMP + "(" + count + ")");

                    saveShot(img, ref.path(), ref.fullName());
                }
        );

        throw new AssertionError("No reference image found, " +
                "actual shot has been transferred to be reference");
    }
}
