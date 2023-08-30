package kinasr.nsr_shot.model;

import org.openqa.selenium.WebElement;

import java.util.ArrayList;
import java.util.List;

public class ShotModel {
    private final List<WebElement> elements = new ArrayList<>();
    private String path;
    private Boolean ignoreSize = false;
    private String name = "";
    private String timestamp = "";
    private Integer width;
    private Integer height;
    private String extension = ".png";

    public List<WebElement> elements() {
        return elements;
    }

    public ShotModel addIgnoreElement(WebElement element) {
        this.elements.add(element);
        return this;
    }

    public ShotModel fullPath(String fullPath) {
        var lastSeparatorIndex = fullPath.lastIndexOf("\\");
        if (lastSeparatorIndex == -1)
            lastSeparatorIndex = fullPath.lastIndexOf("/");

        if (lastSeparatorIndex != -1) {
            this.path = fullPath.substring(0, lastSeparatorIndex + 1);

            if (lastSeparatorIndex < fullPath.length() - 1)
                separateName(fullPath.substring(lastSeparatorIndex + 1));

        } else
            separateName(fullPath);

        return this;
    }

    private void separateName(String fullName) {
        this.extension(fullName.substring(fullName.lastIndexOf(".")));
        fullName = fullName.replace(this.extension, "");

        while (true) {
            var sub = fullName.substring(fullName.lastIndexOf("_") + 1);
            fullName = fullName.replace("_" + sub, "");

            if (sub.contains("x")) {
                this.windowSize(sub);
            } else if (sub.matches("\\d+")) {
                this.timestamp = sub;
            } else {
                this.name = sub;
                break;
            }
        }
    }

    public String fullPath() {
        var fullPath = new StringBuilder(path)
                .append(name);

        if (!timestamp.isEmpty())
            fullPath.append("_")
                    .append(timestamp);

        if (!windowSize().isEmpty())
            fullPath.append("_")
                    .append(windowSize());

        fullPath.append(extension);

        return fullPath.toString();
    }

    public ShotModel path(String path) {
        this.path = path;
        return this;
    }

    public String path() {
        return path;
    }

    public Boolean ignoreSize() {
        return ignoreSize;
    }

    public ShotModel ignoreSize(Boolean ignoreSize) {
        this.ignoreSize = ignoreSize;
        return this;
    }

    public String name() {
        return this.name;
    }

    public ShotModel name(String imageName) {
        this.name = imageName;
        return this;
    }

    public String timestamp() {
        return timestamp;
    }

    public ShotModel timestamp(String timestamp) {
        this.timestamp = timestamp;
        return this;
    }

    public String extension() {
        return extension;
    }

    public ShotModel extension(String extension) {
        this.extension = extension;
        return this;
    }

    public String windowSize() {
        return width != null && height != null ? width + "_" + height : "";
    }

    public ShotModel windowSize(String size) {
        var dimension = size.split("x");

        if (dimension.length == 2) {
            this.width = Integer.valueOf(dimension[0]);
            this.height = Integer.valueOf(dimension[1]);
        } else {
            // bad size should show warning
        }

        return this;
    }

    public Integer width() {
        return width;
    }

    public ShotModel width(Integer width) {
        this.width = width;
        return this;
    }

    public Integer height() {
        return height;
    }

    public ShotModel height(Integer height) {
        this.height = height;
        return this;
    }

    @Override
    public String toString() {
        return "ShotModel{" +
                "elements=" + elements +
                ", path='" + path + '\'' +
                ", ignoreSize=" + ignoreSize +
                ", name='" + name + '\'' +
                ", timestamp='" + timestamp + '\'' +
                ", width=" + width +
                ", height=" + height +
                ", extension='" + extension + '\'' +
                '}';
    }
}
