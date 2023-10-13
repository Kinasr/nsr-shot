package kinasr.nsr_shot.model;

public class TechniqueRecord {
    private SimilarityTechniques technique;
    private Double threshold;
    private Operation comparisonOperation;

    public TechniqueRecord() {
    }

    public TechniqueRecord(SimilarityTechniques technique, Double threshold, Operation comparisonOperation) {
        this.technique = technique;
        this.threshold = threshold;
        this.comparisonOperation = comparisonOperation;
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
        return comparisonOperation;
    }

    public TechniqueRecord operation(Operation operation) {
        this.comparisonOperation = operation;
        return this;
    }
}
