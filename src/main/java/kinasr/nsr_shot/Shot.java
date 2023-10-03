package kinasr.nsr_shot;

import kinasr.nsr_shot.exception.ShotFileException;
import kinasr.nsr_shot.model.ScreenshotModel;
import kinasr.nsr_shot.model.ShotAttribute;
import kinasr.nsr_shot.model.ShotOption;
import kinasr.nsr_shot.shot_manager.ShotMatching;
import kinasr.nsr_shot.shot_manager.TakeRef;
import kinasr.nsr_shot.shot_manager.TakeShot;
import kinasr.nsr_shot.utility.config.ConfigHandler;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;

import static kinasr.nsr_shot.utility.Constant.NAME_SPLITTER;
import static kinasr.nsr_shot.utility.Constant.REF_IMAGE_STAMP;
import static kinasr.nsr_shot.utility.Helper.getFileFullPathWithPrefix;
import static kinasr.nsr_shot.utility.Helper.prepareShotName;

public class Shot {
    private final WebDriver driver;
    private final ShotAttribute attribute;
    private final ShotOption option;

    public Shot(WebDriver driver) {
        this(driver, ShotOption.create());
    }

    public Shot(WebDriver driver, ShotOption option) {
        this.driver = driver;
        this.option = option;

        this.attribute = new ShotAttribute()
                .name(prepareShotName(option.fluentDepth()));
    }

    public Shot ignoreElement(By by) {
        attribute.addLocator(by);
        return this;
    }

    public Shot ignoreElement(By[] by) {
        attribute.addLocators(Arrays.asList(by));
        return this;
    }

    public Shot ignoreElement(WebElement element) {
        attribute.addElement(element);
        return this;
    }

    public Shot ignoreElement(WebElement[] elements) {
        attribute.addElements(Arrays.asList(elements));
        return this;
    }

    public Shot withName(String name) {
        attribute.name(name);
        return this;
    }

    public TakeShot addRef(String referencePath) throws IOException {
        return new TakeShot(
                driver,
                attribute,
                option,
                new ScreenshotModel()
                        .fullPath(referencePath)
                        .image(Files.readAllBytes(Path.of(referencePath)))
        );
    }

    public TakeShot takeRef() {
        return new TakeRef(
                driver,
                attribute,
                option
        ).takeRef();
    }

    public TakeShot takeRef(By by) {
        return new TakeRef(
                driver,
                attribute,
                option
        ).takeRef(by);
    }

    public TakeShot takeRef(WebElement element) {
        return new TakeRef(
                driver,
                attribute,
                option
        ).takeRef(element);
    }

    public ShotMatching takeShot() {
        return new TakeShot(
                driver,
                attribute,
                option,
                loadRef()
        ).takeShot();
    }

    public ShotMatching takeShot(By by) {
        return new TakeShot(
                driver,
                attribute,
                option,
                loadRef()
        ).takeShot(by);
    }

    public ShotMatching takeShot(WebElement element) {
        return new TakeShot(
                driver,
                attribute,
                option,
                loadRef()
        ).takeShot(element);
    }

    private ScreenshotModel loadRef() {
        var fullPath = getFileFullPathWithPrefix(
                ConfigHandler.refPath(),
                attribute.name() + NAME_SPLITTER + REF_IMAGE_STAMP
        );

        if (fullPath.isEmpty())
            return new ScreenshotModel()
                    .path(ConfigHandler.refPath())
                    .name(attribute.name())
                    .timestamp(REF_IMAGE_STAMP);

        try {
            return new ScreenshotModel()
                    .fullPath(fullPath)
                    .image(Files.readAllBytes(Path.of(fullPath)));
        } catch (IOException e) {
            throw new ShotFileException("Can not load this reference image <" + fullPath + ">");
        }
    }
}
