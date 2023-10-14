package kinasr.nsr_shot.utility.config;

import kinasr.nsr_shot.model.Operation;
import kinasr.nsr_shot.model.SimilarityTechniques;
import kinasr.nsr_shot.model.TechniqueRecord;

import java.util.List;

import static kinasr.nsr_shot.utility.config.ConfigFileLoader.configReader;
import static kinasr.nsr_shot.utility.config.ConfigHelper.fetchData;

public class ConfigHandler {
    private static final ConfigRecord<String> actualDirectory =
            new ConfigRecord<>("shot.imageDirectories.actual");
    private static final ConfigRecord<String> expectedDirectory =
            new ConfigRecord<>("shot.imageDirectories.expected");
    private static final ConfigRecord<Integer> retakeShotCount =
            new ConfigRecord<>("shot.retakeShot.count");
    private static final ConfigRecord<Long> retakeShotInterval =
            new ConfigRecord<>("shot.retakeShot.interval");
    private static final ConfigRecord<Integer> multiRefCount =
            new ConfigRecord<>("shot.multiReference.count");
    private static final ConfigRecord<Long> multiRefInterval =
            new ConfigRecord<>("shot.multiReference.interval");
    private static final ConfigRecord<Boolean> saveShotFlag =
            new ConfigRecord<>("shot.flags.saveShotImage");
    private static final ConfigRecord<Boolean> saveOnFlyRefFlag =
            new ConfigRecord<>("shot.flags.saveOnTheFlyReference");
    private static final ConfigRecord<Boolean> supportFluentNamingFlag =
            new ConfigRecord<>("shot.flags.supportFluentNaming");
    private static final ConfigRecord<Boolean> resizeImageFlag =
            new ConfigRecord<>("shot.flags.resizeShotImageToMatchReferenceImageSize");
    private static final ConfigRecord<Boolean> forceResizeWindowFlag =
            new ConfigRecord<>("shot.flags.forceResizeWindowToMatchReference");
    private static final ConfigRecord<List<TechniqueRecord>> techniques =
            new ConfigRecord<>("shot.techniques");

    private ConfigHandler() {
    }

    public static String shotDirectory() {
        return fetchData(actualDirectory, key -> configReader().get(key).asString())
                .orElse("src/test/resources/shot_images/actual/");
    }

    public static String refDirectory() {
        return fetchData(expectedDirectory, key -> configReader().get(key).asString())
                .orElse("src/test/resources/shot_images/expected/");
    }

    public static Integer retakeShotCount() {
        return fetchData(retakeShotCount, key -> configReader().get(key).asInteger())
                .orElse(0);
    }

    public static Long retakeShotInterval() {
        return fetchData(retakeShotInterval, key -> configReader().get(key).asLong())
                .orElse(1000L);
    }

    public static Integer multiRefCount() {
        return fetchData(multiRefCount, key -> configReader().get(key).asInteger())
                .orElse(1);
    }

    public static Long multiRefInterval() {
        return fetchData(multiRefInterval, key -> configReader().get(key).asLong())
                .orElse(1000L);
    }

    public static Boolean saveShot() {
        return fetchData(saveShotFlag, key -> configReader().get(key).asBoolean())
                .orElse(true);
    }

    public static Boolean saveOnFlyRef() {
        return fetchData(saveOnFlyRefFlag, key -> configReader().get(key).asBoolean())
                .orElse(true);
    }

    public static Boolean supportFluentNaming() {
        return fetchData(supportFluentNamingFlag, key -> configReader().get(key).asBoolean())
                .orElse(false);
    }

    public static Boolean resizeImage() {
        return fetchData(resizeImageFlag, key -> configReader().get(key).asBoolean())
                .orElse(true);
    }

    public static Boolean forceResizeWindow() {
        return fetchData(forceResizeWindowFlag, key -> configReader().get(key).asBoolean())
                .orElse(false);
    }

    public static List<TechniqueRecord> techniques() {
        return fetchData(techniques, key -> configReader().get(key).asList(TechniqueRecord.class))
                .orElse(List.of(
                        new TechniqueRecord(SimilarityTechniques.SSI, 0.90, Operation.GREATER),
                        new TechniqueRecord(SimilarityTechniques.MSE, 50.0, Operation.LESS)
                ));
    }
}
