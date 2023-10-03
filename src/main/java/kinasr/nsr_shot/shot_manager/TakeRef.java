package kinasr.nsr_shot.shot_manager;

import kinasr.nsr_shot.model.ScreenshotModel;
import kinasr.nsr_shot.model.ShotAttribute;
import kinasr.nsr_shot.model.ShotOption;
import kinasr.nsr_shot.utility.config.ConfigHandler;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import static kinasr.nsr_shot.utility.Constant.REF_IMAGE_STAMP;
import static kinasr.nsr_shot.utility.Helper.hideUnwantedElements;
import static kinasr.nsr_shot.utility.Helper.saveShot;

public class TakeRef {
    private final WebDriver driver;
    private final ShotAttribute attribute;
    private final ShotOption option;
    private final ScreenshotModel ref = new ScreenshotModel();

    public TakeRef(WebDriver driver,
                    ShotAttribute attribute,
                    ShotOption option) {
        this.driver = driver;
        this.attribute = attribute;
        this.option = option;
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
        setRefModelData(attribute.name());

        return refScreenshot(null);
    }

    public TakeShot takeRef(By by) {
        setRefModelData(attribute.name());

        return refScreenshot(driver.findElement(by));
    }

    public TakeShot takeRef(WebElement element) {
        setRefModelData(attribute.name());

        return refScreenshot(element);
    }

    private void setRefModelData(String name) {
        ref.path(ConfigHandler.refPath())
                .name(name)
                .timestamp(REF_IMAGE_STAMP);
    }

    private TakeShot refScreenshot(WebElement element) {
        hideUnwantedElements(driver, attribute.locators(), attribute.elements());

        var windowSize = driver.manage().window().getSize();
        ref.width(windowSize.width)
                .height(windowSize.height);

        ref.image(
                element == null ?
                        ShotTaker.takeFullShot(driver) :
                        ShotTaker.takeElementShot(element)
        );

        if (ConfigHandler.saveOnFlyRef())
            saveShot(ref.image(), ref.path(), ref.fullName());

        return new TakeShot(
                driver,
                attribute,
                option,
                ref
        );
    }
}