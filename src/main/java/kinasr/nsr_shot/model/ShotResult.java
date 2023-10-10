package kinasr.nsr_shot.model;

import java.util.ArrayList;
import java.util.List;

public class ShotResult {
    private boolean isMatch;
    private final ShotRecord ref;
    private final List<ShotRecord> shots = new ArrayList<>();
    private ShotRecord matchedShot = null;

    public ShotResult(ShotRecord ref) {
        this.ref = ref;
    }

    public ShotResult(boolean isMatch, ShotRecord ref) {
        this.isMatch = isMatch;
        this.ref = ref;
    }

    public ShotResult isMatch(boolean isMatch) {
        this.isMatch = isMatch;
        return this;
    }

    public ShotResult addMatchedShot(ShotRecord matchedShot) {
        this.matchedShot = matchedShot;
        this.shots.add(matchedShot);
        return this;
    }

    public ShotResult addShot(ShotRecord shot) {
        this.shots.add(shot);
        return this;
    }

    public boolean isMatch() {
        return isMatch;
    }

    public ShotRecord ref() {
        return ref;
    }

    public ShotRecord matchedShot() {
        return matchedShot;
    }

    public List<ShotRecord> shots() {
        return shots;
    }

    public void verify() {
        if (Boolean.FALSE.equals(isMatch)) {
            throw new AssertionError("Shot Image not matching the reference");
        }
    }
}
