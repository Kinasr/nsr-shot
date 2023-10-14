package kinasr.nsr_shot;

import kinasr.nsr_shot.exception.ShotFileException;
import kinasr.nsr_shot.model.ScreenshotModel;
import kinasr.nsr_shot.model.ShotAttribute;
import kinasr.nsr_shot.model.ShotOption;
import kinasr.nsr_shot.shot_manager.ShotExecutor;
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

    /**
     * Adds a locator to the attribute.
     *
     * @param by the locator to be added
     * @return the updated Shot object
     */
    public Shot ignoreElement(By by) {
        attribute.addLocator(by);
        return this;
    }

    /**
     * Adds the provided locators to the list of ignored elements.
     *
     * @param by the locators to be ignored
     * @return the updated Shot object
     */
    public Shot ignoreElement(By[] by) {
        attribute.addLocators(Arrays.asList(by));
        return this;
    }

    /**
     * Adds the specified element to the attribute list and returns the current instance.
     *
     * @param element the WebElement to be added
     * @return the current instance of Shot
     */
    public Shot ignoreElement(WebElement element) {
        attribute.addElement(element);
        return this;
    }

    /**
     * Adds the given elements to the attribute list and returns the modified Shot object.
     *
     * @param elements The elements to be added to the attribute list.
     * @return The modified Shot object.
     */
    public Shot ignoreElement(WebElement[] elements) {
        attribute.addElements(Arrays.asList(elements));
        return this;
    }

    /**
     * Sets the name attribute of the Shot.
     *
     * @param name The name to be set.
     * @return The modified Shot object.
     */
    public Shot withName(String name) {
        attribute.name(name);
        return this;
    }

    /**
     * Adds a reference screenshot to the TakeShot object.
     *
     * @param referencePath The path to the reference screenshot.
     * @return The updated TakeShot object.
     * @throws IOException If there is an error reading the reference screenshot file.
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
        return new TakeRef(
                driver,
                attribute,
                option
        ).takeRef();
    }


    /**
     * Takes a screenshot of the element identified by the given selector.
     * @param by The selector used to identify the element.
     * @return A TakeShot object representing the screenshot.
     */
    public TakeShot takeRef(By by) {
        return new TakeRef(
                driver,
                attribute,
                option
        ).takeRef(by);
    }

    /**
     * Takes a screenshot of the given WebElement.
     *
     * @param element The WebElement to take a screenshot of.
     * @return The TakeShot object representing the screenshot.
     */
    public TakeShot takeRef(WebElement element) {
        return new TakeRef(
                driver,
                attribute,
                option
        ).takeRef(element);
    }

    /**
     * Takes a shot using the provided driver, attribute, option, and loaded reference.
     *
     * @return The shot executor.
     */
    public ShotExecutor takeShot() {
        return new TakeShot(
                driver,
                attribute,
                option,
                loadRef()
        ).takeShot();
    }

    /**
     * Takes a screenshot of the element specified by the given locator.
     *
     * @param by the locator used to find the element to take a screenshot of
     * @return a ShotExecutor object that can be used to perform actions on the screenshot
     */
    public ShotExecutor takeShot(By by) {
        return new TakeShot(
                driver,
                attribute,
                option,
                loadRef()
        ).takeShot(by);
    }

    /**
     * Takes a screenshot of the specified WebElement.
     *
     * @param element The WebElement to take a screenshot of.
     * @return A ShotExecutor object for further operations on the screenshot.
     */
    public ShotExecutor takeShot(WebElement element) {
        return new TakeShot(
                driver,
                attribute,
                option,
                loadRef()
        ).takeShot(element);
    }

    /**
     * Loads the reference screenshot model.
     *
     * @return the reference screenshot model
     * @throws ShotFileException if the reference image cannot be loaded
     */
    private ScreenshotModel loadRef() {
        var fullPath = getFileFullPathWithPrefix(
                ConfigHandler.refDirectory(),
                attribute.name() + NAME_SPLITTER + REF_IMAGE_STAMP + NAME_SPLITTER
        );

        if (fullPath.isEmpty())
            return new ScreenshotModel()
                    .path(ConfigHandler.refDirectory())
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
