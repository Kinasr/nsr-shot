package kinasr.nsr_shot.model;

public class TechniqueRecord {
    private SimilarityTechniques technique;
    private Double threshold;
    private Operation operation;

    public TechniqueRecord() {
    }

    public TechniqueRecord(SimilarityTechniques technique, Double threshold, Operation operation) {
        this.technique = technique;
        this.threshold = threshold;
        this.operation = operation;
    }

    public SimilarityTechniques technique() {
        return technique;
    }

    public TechniqueRecord technique(SimilarityTechniques technique) {
        this.technique = technique;
        return this;
    }

    public Double threshold() {
        return threshold;
    }

    public TechniqueRecord threshold(Double threshold) {
        this.threshold = threshold;
        return this;
    }

    public Operation operation() {
        return operation;
    }

    public TechniqueRecord operation(Operation operation) {
        this.operation = operation;
        return this;
    }
}
