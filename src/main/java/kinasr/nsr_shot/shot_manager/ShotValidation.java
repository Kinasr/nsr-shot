package kinasr.nsr_shot.shot_manager;

import kinasr.nsr_shot.cv_proccess.CVManager;
import kinasr.nsr_shot.model.Operation;
import kinasr.nsr_shot.model.ShotModel;
import kinasr.nsr_shot.model.SimilarityTechniques;
import kinasr.nsr_shot.model.TechniqueRecord;
import kinasr.nsr_shot.utility.Helper;
import kinasr.nsr_shot.utility.config.ConfigHandler;
import org.openqa.selenium.WebDriver;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

public class ShotValidation {
    private static final Logger logger = LoggerFactory.getLogger(ShotValidation.class);
    private final WebDriver driver;
    private final ShotModel model;
    private List<TechniqueRecord> techniques = new ArrayList<>();

    public ShotValidation(WebDriver driver, ShotModel model) {
        this.driver = driver;
        this.model = model;
    }

    public ShotValidation technique(SimilarityTechniques technique, Double threshold, Operation operation) {
        techniques.add(new TechniqueRecord(
                technique,
                threshold,
                operation
        ));

        return this;
    }

    public void assertThatShotMatchReference() {
        var expectedShotPath = ConfigHandler.expectedPath()
                + model.name() + "ref.png";

        assertThatShotMatchReference(expectedShotPath);

    }

    public void assertThatShotMatchReference(String expectedShotPath) {
        if (techniques.isEmpty())
            techniques = ConfigHandler.techniques();

        if (Boolean.FALSE.equals(Helper.isFileExist(expectedShotPath))){
            Helper.moveAndRenameFile(model.fullPath(), expectedShotPath);

            throw new AssertionError("No reference image found, " +
                    "actual shot has been transferred to be reference");
        }

        var cv = new CVManager(model.fullPath(), expectedShotPath);

        var shotSize = cv.image1Size();
        var refSize = cv.image2Size();
        if (Boolean.TRUE.equals(model.ignoreSize()))
            cv.resizeImg2ToMatchImg1();
        else if (!shotSize.equals(refSize))
            throw new AssertionError("The two images are not the same size. - Shot size <" +
                    shotSize.width() + "*" + shotSize.height() + "> - Reference size <" +
                    refSize.width() + "*" + refSize.height() + ">");

        var result = true;
        var msg = new StringBuilder();
        for (TechniqueRecord techniqueRecord : techniques) {
            var diff = cv.getDiff(techniqueRecord.technique());
            result = result && techniqueRecord.operation().operationResult(diff, techniqueRecord.threshold());
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

        cv.close();
        if (Boolean.FALSE.equals(result))
            throw new AssertionError("Image not match the reference using [\n" + msg);

        logger.info("Assert that shot matching the reference using [\n{}", msg);
    }

    private void saveRefImageAndThrow(ShotModel shot, String refPath) {
        var windowSize = driver.manage().window().getSize();

        shot.width(windowSize.width)
                        .height(windowSize.height);

        Helper.moveAndRenameFile(shot.fullPath(), refPath);

        throw new AssertionError("No reference image found, " +
                "actual shot has been transferred to be reference");
    }
}
