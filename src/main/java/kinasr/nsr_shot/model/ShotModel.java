package kinasr.nsr_shot.model;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static kinasr.nsr_shot.utility.Helper.separateFullPath;

@SuppressWarnings("UnusedReturnValue")
public class ShotModel {
    private static final Logger logger = LoggerFactory.getLogger(ShotModel.class);
    private String path;
    private String name = "";
    private String timestamp = "";
    private Integer width;
    private Integer height;
    private String extension = ".png";
    private Boolean doesDirectoryCreated = false;
    private byte[] image;

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
            logger.warn("This is an invalid size <{}> size should be in format <800x600>", size);
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

    public Boolean doesDirectoryCreated() {
        return doesDirectoryCreated;
    }

    public ShotModel doesDirectoryCreated(Boolean doesDirectoryCreated) {
        this.doesDirectoryCreated = doesDirectoryCreated;
        return this;
    }

    public byte[] image() {
        return image;
    }

    public ShotModel image(byte[] image) {
        this.image = image;
        return this;
    }

    @Override
    public String toString() {
        return "ShotModel{" +
                "path='" + path + '\'' +
                ", name='" + name + '\'' +
                ", timestamp='" + timestamp + '\'' +
                ", width=" + width +
                ", height=" + height +
                ", extension='" + extension + '\'' +
                ", doesDirectoryCreated=" + doesDirectoryCreated +
                '}';
    }
}
