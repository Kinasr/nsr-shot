package kinasr.nsr_shot.utility.config;

import kinasr.nsr_shot.model.Operation;
import kinasr.nsr_shot.model.SimilarityTechniques;
import kinasr.nsr_shot.model.TechniqueRecord;

import java.util.List;

import static kinasr.nsr_shot.utility.config.ConfigFileLoader.configReader;
import static kinasr.nsr_shot.utility.config.ConfigHelper.fetchData;

public class ConfigHandler {
    private static final ConfigRecord<String> actualPath = new ConfigRecord<>("shot.actual-path");
    private static final ConfigRecord<String> expectedPath = new ConfigRecord<>("shot.expected-path");
    private static final ConfigRecord<Boolean> supportFluent = new ConfigRecord<>("shot.support-fluent");
    private static final ConfigRecord<Boolean> resizeImage =
            new ConfigRecord<>("shot.resize-ref-image-to-match-shot-image-size");
    private static final ConfigRecord<Boolean> forceResizeWindow =
            new ConfigRecord<>("shot.force-resize-window-to-match-the-reference");
    private static final ConfigRecord<List<TechniqueRecord>> techniques = new ConfigRecord<>("shot.techniques");

    private ConfigHandler() {
    }

    public static String shotPath() {
        return fetchData(actualPath, key -> configReader().get(key).asString())
                .orElse("src/test/resources/shot_images/actual/");
    }

    public static String refPath() {
        return fetchData(expectedPath, key -> configReader().get(key).asString())
                .orElse("src/test/resources/shot_images/expected/");
    }

    public static Boolean supportFluent() {
        return fetchData(supportFluent, key -> configReader().get(key).asBoolean())
                .orElse(false);
    }

    public static Boolean resizeImage() {
        return fetchData(resizeImage, key -> configReader().get(key).asBoolean())
                .orElse(true);
    }

    public static Boolean forceResizeWindow() {
        return fetchData(forceResizeWindow, key -> configReader().get(key).asBoolean())
                .orElse(false);
    }

    public static List<TechniqueRecord> techniques() {
        return fetchData(techniques, key -> configReader().get(key).asList(TechniqueRecord.class))
                .orElse(List.of(
                        new TechniqueRecord(SimilarityTechniques.SSI, 0.999, Operation.GREATER),
                        new TechniqueRecord(SimilarityTechniques.MSE, 1.0, Operation.LESS)
                ));
    }
}
