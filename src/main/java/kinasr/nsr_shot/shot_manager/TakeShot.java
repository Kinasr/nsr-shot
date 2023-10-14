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

    /**
     * Takes a shot using the shot executor.
     *
     * @return The shot executor instance.
     */
    public ShotExecutor takeShot() {
        setShotModelData(attribute.name());

        return shotExecutor(null);
    }

    /**
     * Takes a screenshot of the element specified by the given locator.
     *
     * @param by the locator of the element to take a screenshot of
     * @return the ShotExecutor instance to configure and execute the screenshot action
     */
    public ShotExecutor takeShot(By by) {
        setShotModelData(attribute.name());

        return shotExecutor(driver.findElement(by));
    }

    /**
     * Takes a screenshot of the given web element.
     *
     * @param element The web element to take a screenshot of.
     * @return The shot executor object.
     */
    public ShotExecutor takeShot(WebElement element) {
        setShotModelData(attribute.name());

        return shotExecutor(element);
    }

    /**
     * Sets the shot model data.
     *
     * @param name the name of the shot
     */
    private void setShotModelData(String name) {
        shot.path(ConfigHandler.shotDirectory())
                .name(name)
                .timestamp(timestamp());
    }

    /**
     * Executes a shot on a given WebElement.
     *
     * @param element The WebElement on which the shot will be executed.
     * @return The ShotExecutor responsible for executing the shot.
     */
    private ShotExecutor shotExecutor(WebElement element) {
        prepareWindowSize();
        hideUnwantedElements(driver, attribute.locators(), attribute.elements());

        return new ShotExecutor(driver, element, ref, shot, option);
    }

    /**
     * Prepares the window size for taking a screenshot.
     */
    private void prepareWindowSize() {
        var windowSize = driver.manage().window().getSize();
        shot.width(windowSize.width)
                .height(windowSize.height);


        if (ref.isLoaded()) {
            if (Boolean.TRUE.equals(!ref.windowSize().isEmpty() && option.forceResizeWindow()) &&
                    !shot.windowSize().equals(ref.windowSize())) {
                driver.manage().window().setSize(new Dimension(ref.width(), ref.height()));

                shot.width(ref.width())
                        .height(ref.height());
            } else if (Boolean.TRUE.equals(option.forceResizeWindow() && ref.windowSize().isEmpty())) {
                var refFullPath = ref.fullPath();
                logger.warn("Can not resize window, can not retrieve size from this Ref image <{}>", refFullPath);
            }
        }
    }
}
