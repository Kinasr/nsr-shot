package kinasr.nsr_shot.model;

import java.util.ArrayList;
import java.util.List;

public class MatchingResult {
    private final boolean isMatch;
    private final ShotRecord ref;
    private final List<ShotRecord> shots = new ArrayList<>();
    private ShotRecord matchedShot = null;

    public MatchingResult(boolean isMatch, ShotRecord ref) {
        this.isMatch = isMatch;
        this.ref = ref;
    }

    public MatchingResult addMatchedShot(ShotRecord matchedShot) {
        this.matchedShot = matchedShot;
        this.shots.add(matchedShot);
        return this;
    }

    public MatchingResult addShot(ShotRecord shot) {
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
}
