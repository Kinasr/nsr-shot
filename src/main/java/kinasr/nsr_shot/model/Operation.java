package kinasr.nsr_shot.model;

import java.util.function.BiPredicate;

public enum Operation {
    LESS((a, e) -> a <= e), GREATER((a, e) -> a >= e);

    private final BiPredicate<Double, Double> op;

    Operation(BiPredicate<Double, Double> op) {
        this.op = op;
    }

    public Boolean operationResult(Double actual, Double expected) {
        return op.test(actual, expected);
    }
}
