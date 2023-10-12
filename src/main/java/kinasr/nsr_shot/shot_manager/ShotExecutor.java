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

    public ShotExecutor technique(SimilarityTechniques technique, Double threshold, Operation operation) {
        techniques.add(new TechniqueRecord(
                technique,
                threshold,
                operation
        ));

        return this;
    }

    public ShotResult perform() {
        result = new ShotResult(new ShotRecord(ref.image(), ref.fullPath()));

        var isMatch = Helper.await(
                ConfigHandler.retakeShot(),
                ConfigHandler.retakeShotInterval(),
                this::isMatch
        );

        return result.isMatch(isMatch);
    }

    public void verify() {
        perform().verify();
    }

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

    private CVManager screenshot() {
        shot.image(element == null ?
                ShotTaker.takeFullShot(driver) :
                ShotTaker.takeElementShot(element));

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

    private void saveRefAndThrow() {
        ref.width(shot.width()).height(shot.height());
        saveShot(shot.image(), ref.path(), ref.fullName());

        throw new AssertionError("No reference image found, " +
                "actual shot has been transferred to be reference");
    }
}
