package kinasr.nsr_shot.utility.config;

public final class ConfigRecord<T> {
    private final String key;
    private T value;
    private Boolean isExist = false;
    private Boolean isFetched = false;

    public ConfigRecord(String key) {
        this.key = key;
    }

    public String key() {
        return key;
    }

    public T value() {
        return value;
    }

    public ConfigRecord<T> value(T value) {
        this.value = value;
        return this;
    }

    public Boolean isExist() {
        return isExist;
    }

    public ConfigRecord<T> isExist(Boolean exist) {
        isExist = exist;
        return this;
    }

    public Boolean isFetched() {
        return isFetched;
    }

    public ConfigRecord<T> isFetched(Boolean fetched) {
        isFetched = fetched;
        return this;
    }
}
