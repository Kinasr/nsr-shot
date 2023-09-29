package kinasr.nsr_shot.shot_manager;

import kinasr.nsr_shot.cv_proccess.CVManager;
import kinasr.nsr_shot.model.*;
import kinasr.nsr_shot.utility.config.ConfigHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

public class ShotMatching {
    private static final Logger logger = LoggerFactory.getLogger(ShotMatching.class);
    private final ShotModel shotModel;
    private final ShotModel refModel;
    private final Boolean resizeImage;
    private List<TechniqueRecord> techniques = new ArrayList<>();

    public ShotMatching(ShotModel shotModel, ShotModel refModel, Boolean resizeImage) {
        this.shotModel = shotModel;
        this.refModel = refModel;
        this.resizeImage = resizeImage;
    }

    public ShotMatching technique(SimilarityTechniques technique, Double threshold, Operation operation) {
        techniques.add(new TechniqueRecord(
                technique,
                threshold,
                operation
        ));

        return this;
    }

    public void assertThatShotMatchReference() {
        if (Boolean.FALSE.equals(isMatch().isMatch())) {
            throw new AssertionError("Shot Image not matching the reference");
        }
    }

    public MatchingResult isMatch() {
        var isMatch = true;
        if (techniques.isEmpty())
            techniques = ConfigHandler.techniques();

        var cv = new CVManager(shotModel.image(), refModel.image());
        var shotSize = cv.image1Size();
        var refSize = cv.image2Size();

        if (Boolean.TRUE.equals(resizeImage))
            cv.resizeImg2ToMatchImg1();
        else if (!cv.isTheTwoImagesHaveTheSameSize())
            throw new AssertionError("The two images are not the same size. - Shot size <" +
                    shotSize.width() + "*" + shotSize.height() + "> - Reference size <" +
                    refSize.width() + "*" + refSize.height() + ">");

        var msg = new StringBuilder();
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
        cv.close();

        var result = new MatchingResult(
                isMatch,
                new ShotRecord(refModel.image(), refModel.fullPath())
        );
        if (Boolean.TRUE.equals(isMatch)) {
            result.addMatchedShot(new ShotRecord(shotModel.image(), shotModel.fullPath()));
            logger.info("Assert that shot matching the reference using [\n{}", msg);
        } else {
            result.addShot(new ShotRecord(shotModel.image(), shotModel.fullPath()));
            logger.warn("Image not match the reference using [\n{}", msg);
        }

        return result;
    }
}
