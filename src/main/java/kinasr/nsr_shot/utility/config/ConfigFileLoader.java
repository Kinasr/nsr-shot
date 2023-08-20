package kinasr.nsr_shot.utility.config;

import exception.YAMLFileException;
import nsr_yaml.YAML;
import nsr_yaml.YAMLReader;

public class ConfigFileLoader {
    private static YAMLReader configFile;
    private final YAMLReader reader;

    private ConfigFileLoader() {
        YAMLReader r;
        try {
            r = YAML.read("src/main/resources/config.yaml");
        } catch (YAMLFileException e) {
            throw new YAMLFileException("Failed to access 'config.yaml' " +
                    "file or file not found in src/main/resources directory", e);
        }

        reader = r;
    }

    public static YAMLReader configReader() {
        if (configFile == null) {
            configFile = new ConfigFileLoader().reader;
        }

        return configFile;
    }

}
