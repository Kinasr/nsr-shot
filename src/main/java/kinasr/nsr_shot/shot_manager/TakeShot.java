package kinasr.nsr_shot.shot_manager;

import kinasr.nsr_shot.model.ScreenshotModel;
import kinasr.nsr_shot.model.ShotAttribute;
import kinasr.nsr_shot.model.ShotOption;
import kinasr.nsr_shot.utility.config.ConfigHandler;
import org.openqa.selenium.By;
import org.openqa.selenium.Dimension;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static kinasr.nsr_shot.utility.Helper.*;

public class TakeShot {
    private static final Logger logger = LoggerFactory.getLogger(TakeShot.class);
    private final WebDriver driver;
    private final ShotAttribute attribute;
    private final ShotOption option;
    private final ScreenshotModel ref;
    private final ScreenshotModel shot = new ScreenshotModel();

    public TakeShot(WebDriver driver,
                    ShotAttribute attribute,
                    ShotOption option,
                    ScreenshotModel ref) {
        this.driver = driver;
        this.attribute = attribute;
        this.option = option;
        this.ref = ref;
    }

    public ShotMatching takeShot() {
        setShotModelData(attribute.name());

        return screenshot(null);
    }

    public ShotExecutor takeShot2() {
        setShotModelData(attribute.name());

        return shotExecutor(null);
    }

    public ShotMatching takeShot(By by) {
        setShotModelData(attribute.name());

        return screenshot(driver.findElement(by));
    }

    public ShotMatching takeShot(WebElement element) {
        setShotModelData(attribute.name());

        return screenshot(element);
    }

    private void setShotModelData(String name) {
        shot.path(ConfigHandler.shotPath())
                .name(name)
                .timestamp(timestamp());
    }

    private ShotExecutor shotExecutor(WebElement element) {
        prepareWindowSize();
        hideUnwantedElements(driver, attribute.locators(), attribute.elements());

        return new ShotExecutor(driver, element, ref, shot, option);
    }

    private ShotMatching screenshot(WebElement element) {
        prepareWindowSize();
        hideUnwantedElements(driver, attribute.locators(), attribute.elements());

        shot.image(
                element == null ?
                        ShotTaker.takeFullShot(driver) :
                        ShotTaker.takeElementShot(element)
        );

        if (!ref.isLoaded())
            saveRefAndThrow();

        if (ConfigHandler.saveShot())
            saveShot(shot.image(), shot.path(), shot.fullName());

        return new ShotMatching(shot, ref, option.resizeImage());
    }


    private void prepareWindowSize() {
        var windowSize = driver.manage().window().getSize();
        shot.width(windowSize.width)
                .height(windowSize.height);


        if (ref.isLoaded()) {
            if (Boolean.TRUE.equals(!ref.windowSize().isEmpty() && option.forceResizeWindow()) &&
                    !shot.windowSize().equals(ref.windowSize())) {
                // Don't know why, but I need to -1 from width
                driver.manage().window().setSize(new Dimension(ref.width() - 1, ref.height()));

                shot.width(ref.width())
                        .height(ref.height());
            } else if (Boolean.TRUE.equals(option.forceResizeWindow() && ref.windowSize().isEmpty())) {
                var refFullPath = ref.fullPath();
                logger.warn("Can not resize window, can not retrieve size from this Ref image <{}>", refFullPath);
            }
        }
    }

    private void saveRefAndThrow() {
        ref.width(shot.width()).height(shot.height());
        saveShot(shot.image(), ref.path(), ref.fullName());

        throw new AssertionError("No reference image found, " +
                "actual shot has been transferred to be reference");
    }
}
