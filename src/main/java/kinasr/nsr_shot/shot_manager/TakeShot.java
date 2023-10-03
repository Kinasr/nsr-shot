package kinasr.nsr_shot.shot_manager;

import kinasr.nsr_shot.model.ScreenshotModel;
import kinasr.nsr_shot.model.ShotAttribute;
import kinasr.nsr_shot.model.ShotOption;
import kinasr.nsr_shot.utility.Helper;
import kinasr.nsr_shot.utility.config.ConfigHandler;
import org.openqa.selenium.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

import static kinasr.nsr_shot.utility.Helper.hideUnwantedElements;
import static kinasr.nsr_shot.utility.Helper.timestamp;

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

    public ShotMatching takeShot(By by) {
        setShotModelData(attribute.name());

        return screenshot(driver.findElement(by));
    }

    public ShotMatching takeShot(WebElement element) {
        setShotModelData(attribute.name());

        return screenshot(element);
    }

    private ShotMatching screenshot(WebElement element) {
        hideUnwantedElements(driver, attribute.locators(), attribute.elements());

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

        shot.image(
                element == null ?
                        ShotTaker.takeFullShot(driver, shot) :
                        ShotTaker.takeElementShot(shot, element)
        );

        if (!ref.isLoaded()){
            ref.width(shot.width()).height(shot.height());
            saveRefImageAndThrow();
        }

        return new ShotMatching(shot, ref, option.resizeImage());
    }

    private void setShotModelData(String name) {
        shot.path(ConfigHandler.shotPath())
                .name(name)
                .timestamp(timestamp());
    }

    private void saveRefImageAndThrow() {
        Helper.moveAndRenameFile(shot.fullPath(), ref.path(), ref.fullName());

        throw new AssertionError("No reference image found, " +
                "actual shot has been transferred to be reference");
    }
}
