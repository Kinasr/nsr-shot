package kinasr.nsr_shot.model;

import org.openqa.selenium.WebElement;

import java.util.ArrayList;
import java.util.List;

public class ShotModel {
    private final List<WebElement> elements = new ArrayList<>();
    private String actualScreenshotPath;
    private Boolean ignoreSize = false;
    private String imageName = "";

    public List<WebElement> elements() {
        return elements;
    }

    public ShotModel addIgnoreElement(WebElement element) {
        this.elements.add(element);
        return this;
    }

    public String actualShotPath() {
        return actualScreenshotPath;
    }

    public ShotModel actualShotPath(String actualScreenshotPath) {
        this.actualScreenshotPath = actualScreenshotPath;
        return this;
    }

    public Boolean ignoreSize() {
        return ignoreSize;
    }

    public ShotModel ignoreSize(Boolean ignoreSize) {
        this.ignoreSize = ignoreSize;
        return this;
    }

    public String imageName() {
        return this.imageName;
    }

    public ShotModel imageName(String imageName) {
        this.imageName = imageName;
        return this;
    }
}
