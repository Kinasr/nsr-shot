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

import static kinasr.nsr_shot.utility.Constant.REFERENCE_IMAGE_STAMP;
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

    /**
     * Adds a reference screenshot to the TakeShot object.
     *
     * @param referencePath the path to the reference screenshot image
     * @return the updated TakeShot object with the reference screenshot added
     * @throws IOException if there is an error reading the image file
     */
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

    /**
     * Takes a reference shot.
     *
     * @return The reference shot.
     */
    public TakeShot takeRef() {
        setRefModelData(attribute.name());

        return refScreenshot(null);
    }

    /**
     * Takes a screenshot of the element located by the given By object.
     *
     * @param by the By object representing the element to be captured
     * @return a TakeShot object containing the captured screenshot
     */
    public TakeShot takeRef(By by) {
        setRefModelData(attribute.name());

        return refScreenshot(driver.findElement(by));
    }

    /**
     * Takes a reference screenshot of the specified web element.
     * This method sets the reference model data and returns the reference screenshot.
     *
     * @param element The web element to take a reference screenshot of.
     * @return The reference screenshot.
     */
    public TakeShot takeRef(WebElement element) {
        setRefModelData(attribute.name());

        return refScreenshot(element);
    }

    /**
     * Sets the reference model data for a given name.
     *
     * @param name The name of the reference model.
     */
    private void setRefModelData(String name) {
        ref.path(ConfigHandler.refDirectory())
                .name(name)
                .timestamp(REFERENCE_IMAGE_STAMP);
    }

    /**
     * Takes a screenshot of the given web element and returns a TakeShot object.
     *
     * @param element The web element to take a screenshot of. If null, takes a full page screenshot.
     * @return The TakeShot object containing the screenshot and other information.
     */
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