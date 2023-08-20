package kinasr.nsr_shot.utility.config;

import kinasr.nsr_shot.model.Operation;
import kinasr.nsr_shot.model.SimilarityTechniques;
import kinasr.nsr_shot.model.TechniqueRecord;

import java.util.List;
import java.util.Optional;

import static kinasr.nsr_shot.utility.config.ConfigFileLoader.configReader;
import static kinasr.nsr_shot.utility.config.ConfigHelper.fetchData;

public class ConfigHandler {
    private static final ConfigRecord<String> actualPath = new ConfigRecord<>("shot.actual-path");
    private static final ConfigRecord<String> expectedPath = new ConfigRecord<>("shot.expected-path");
    private static final ConfigRecord<List<TechniqueRecord>> techniques = new ConfigRecord<>("shot.techniques");

    private ConfigHandler() {}

    public static Optional<String> actualPath() {
        return fetchData(actualPath, key -> configReader().get(key).asString());
    }

    public static Optional<String> expectedPath() {
        return fetchData(expectedPath, key -> configReader().get(key).asString());
    }

    public static List<TechniqueRecord> techniques() {
        return fetchData(techniques, key -> configReader().get(key).asList(TechniqueRecord.class))
                .orElse(List.of(
                        new TechniqueRecord(SimilarityTechniques.SSI, 0.999, Operation.GREATER),
                        new TechniqueRecord(SimilarityTechniques.MSE, 1.0, Operation.LESS)
                ));
    }
}
