package kinasr.nsr_shot.model;

import org.openqa.selenium.WebElement;

import java.util.ArrayList;
import java.util.List;

import static kinasr.nsr_shot.utility.Helper.separateFullPath;

public class ShotModel {
    private final List<WebElement> elements = new ArrayList<>();
    private String path;
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
        ShotModel tempModel = separateFullPath(fullPath);

        this.path = tempModel.path;
        this.name = tempModel.name;
        this.timestamp = tempModel.timestamp;
        this.width = tempModel.width;
        this.height = tempModel.height;
        this.extension = tempModel.extension;

        return this;
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

    public String fullName() {
        var fullName = new StringBuilder(name);

        if (!timestamp.isEmpty())
            fullName.append("_")
                    .append(timestamp);

        if (!windowSize().isEmpty())
            fullName.append("_")
                    .append(windowSize());

        fullName.append(extension);

        return fullName.toString();
    }

    public ShotModel path(String path) {
        this.path = path;
        return this;
    }

    public String path() {
        return path;
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
        return width != null && height != null ? width + "x" + height : "";
    }

    public ShotModel windowSize(String size) {
        var dimension = size.split("x");

        if (dimension.length == 2) {
            this.width = Integer.valueOf(dimension[0]);
            this.height = Integer.valueOf(dimension[1]);
        } else {
            // TODO: 08/31/2023   bad size should show warning
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
                ", name='" + name + '\'' +
                ", timestamp='" + timestamp + '\'' +
                ", width=" + width +
                ", height=" + height +
                ", extension='" + extension + '\'' +
                '}';
    }
}
