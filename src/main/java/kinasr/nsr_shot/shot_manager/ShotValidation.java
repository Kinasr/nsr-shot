package kinasr.nsr_shot.shot_manager;

import kinasr.nsr_shot.cv_proccess.CVManager;
import kinasr.nsr_shot.model.Operation;
import kinasr.nsr_shot.model.ShotModel;
import kinasr.nsr_shot.model.SimilarityTechniques;
import kinasr.nsr_shot.model.TechniqueRecord;
import kinasr.nsr_shot.utility.config.ConfigHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

public class ShotValidation {
    private static final Logger logger = LoggerFactory.getLogger(ShotValidation.class);
    private final ShotModel shotModel;
    private final ShotModel refModel;
    private final Boolean doNotResize;
    private List<TechniqueRecord> techniques = new ArrayList<>();

    public ShotValidation(ShotModel shotModel, ShotModel refModel, Boolean doNotResize) {
        this.shotModel = shotModel;
        this.refModel = refModel;
        this.doNotResize = doNotResize;
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
        if (techniques.isEmpty())
            techniques = ConfigHandler.techniques();

        var cv = new CVManager(shotModel.fullPath(), refModel.fullPath());

        var shotSize = cv.image1Size();
        var refSize = cv.image2Size();
        if (Boolean.FALSE.equals(doNotResize))
            cv.resizeImg2ToMatchImg1();
        else if (!cv.isTheTwoImagesHaveTheSameSize())
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
}
