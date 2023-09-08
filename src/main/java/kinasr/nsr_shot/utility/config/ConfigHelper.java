package kinasr.nsr_shot.utility.config;


import kinasr.nsr_yaml.exception.InvalidKeyException;

import java.util.Optional;
import java.util.function.Function;

public class ConfigHelper {
    private ConfigHelper() {
    }

    protected static <T> Optional<T> fetchData(ConfigRecord<T> config,
                                               Function<String, T> reader) {

        if (Boolean.FALSE.equals(config.isFetched())) {
            try {
                config.value(reader.apply(config.key()));
                config.isExist(true);
            } catch (InvalidKeyException ignore) {
                // Ignore missing config file
            } finally {
                config.isFetched(true);
            }
        }

        return Optional.ofNullable(config.value());
    }
}

