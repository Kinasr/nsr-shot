package kinasr.nsr_shot;

import kinasr.nsr_shot.model.ShotModel;
import kinasr.nsr_shot.shot_manager.ShotTacker;
import kinasr.nsr_shot.shot_manager.ShotValidation;
import kinasr.nsr_shot.utility.Helper;
import kinasr.nsr_shot.utility.config.ConfigHandler;
import org.openqa.selenium.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

import static kinasr.nsr_shot.utility.Constant.NAME_SPLITTER;
import static kinasr.nsr_shot.utility.Constant.REF_IMAGE_STAMP;
import static kinasr.nsr_shot.utility.Helper.*;

public class Shot {
    private static final Logger logger = LoggerFactory.getLogger(Shot.class);
    private final WebDriver driver;
    private final ShotModel shotModel = new ShotModel();
    private final ShotModel refModel = new ShotModel();
    private final List<By> elementsLocators = new ArrayList<>();
    private final List<WebElement> elements = new ArrayList<>();
    private final String shotPath;
    private final String refPath;
    private Boolean doNotResize = false;
    private Boolean forceResizeWindow = false;
    private Boolean supportFluent = false;
    private Boolean safeRef = false;

    public Shot(WebDriver driver) {
        this.driver = driver;
        this.shotPath = ConfigHandler.shotPath();
        this.refPath = ConfigHandler.refPath();
    }

    public Shot(WebDriver driver, String shotPath, String refPath) {
        this.driver = driver;
        this.shotPath = shotPath;
        this.refPath = refPath;
    }

    public Shot doNotResize() {
        this.doNotResize = true;
        return this;
    }

    public Shot forceResizeWindowToMatchRef() {
        this.forceResizeWindow = true;
        return this;
    }

    public Shot supportFluent() {
        this.supportFluent = true;
        return this;
    }

    public Shot ignoreElement(By by) {
        elementsLocators.add(by);
        return this;
    }

    public Shot ignoreElement(WebElement element) {
        elements.add(element);
        return this;
    }

    public Shot takeRef() {
        safeRef = false;
        setRefModelData(prepareName());
        safeRef = true;

        return refScreenshot(null);
    }

    public Shot takeRef(String name) {
        safeRef = false;
        setRefModelData(prepareName(name));
        safeRef = true;

        return refScreenshot(null);
    }

    public Shot takeRef(String className, String testName) {
        safeRef = false;
        setRefModelData(prepareName(className, testName));
        safeRef = true;

        return refScreenshot(null);
    }

    public ShotValidation takeShot() {
        var name = prepareName();
        setRefModelData(name);
        setShotModelData(name);

        return screenshot(null);
    }

    public ShotValidation takeShot(String name) {
        var n = prepareName(name);
        setRefModelData(n);
        setShotModelData(n);

        return screenshot(null);
    }

    public ShotValidation takeShot(String className, String testName) {
        var name = prepareName(className, testName);
        setRefModelData(name);
        setShotModelData(name);

        return screenshot(null);
    }

    public Shot takeRef(WebElement element) {
        safeRef = false;
        setRefModelData(prepareName());
        safeRef = true;

        return refScreenshot(element);
    }

    public Shot takeRef(WebElement element, String name) {
        safeRef = false;
        setRefModelData(prepareName(name));
        safeRef = true;

        return refScreenshot(element);
    }

    public Shot takeRef(WebElement element, String className, String testName) {
        safeRef = false;
        setRefModelData(prepareName(className, testName));
        safeRef = true;

        return refScreenshot(element);
    }

    public ShotValidation takeShot(WebElement element) {
        var name = prepareName();
        setRefModelData(name);
        setShotModelData(name);

        return screenshot(element);
    }

    public ShotValidation takeShot(WebElement element, String name) {
        var n = prepareName(name);
        setRefModelData(n);
        setShotModelData(n);

        return screenshot(element);
    }

    public ShotValidation takeShot(WebElement element, String className, String testName) {
        var name = prepareName(className, testName);
        setRefModelData(name);
        setShotModelData(name);

        return screenshot(element);
    }

    public Shot takeRef(By by) {
        safeRef = false;
        setRefModelData(prepareName());
        safeRef = true;

        return refScreenshot(driver.findElement(by));
    }

    public Shot takeRef(By by, String name) {
        safeRef = false;
        setRefModelData(prepareName(name));
        safeRef = true;

        return refScreenshot(driver.findElement(by));
    }

    public Shot takeRef(By by, String className, String testName) {
        safeRef = false;
        setRefModelData(prepareName(className, testName));
        safeRef = true;

        return refScreenshot(driver.findElement(by));
    }

    public ShotValidation takeShot(By by) {
        var name = prepareName();
        setRefModelData(name);
        setShotModelData(name);

        return screenshot(driver.findElement(by));
    }

    public ShotValidation takeShot(By by, String name) {
        var n = prepareName(name);
        setRefModelData(n);
        setShotModelData(n);

        var element = driver.findElement(by);
        return screenshot(element);
    }

    public ShotValidation takeShot(By by, String className, String testName) {
        var name = prepareName(className, testName);
        setRefModelData(name);
        setShotModelData(name);

        var element = driver.findElement(by);
        return screenshot(element);
    }

    private Shot refScreenshot(WebElement element) {
        prepareScreen();

        var windowSize = driver.manage().window().getSize();
        refModel.width(windowSize.width)
                .height(windowSize.height);

        if (element == null)
            ShotTacker.takeFullShot(driver, refModel);
        else
            ShotTacker.takeElementShot(refModel, element);

        return this;
    }

    private ShotValidation screenshot(WebElement element) {
        prepareScreen();
        var windowSize = driver.manage().window().getSize();
        shotModel.width(windowSize.width)
                .height(windowSize.height);


        var isRefExist = safeRef || loadRefData();
        if (Boolean.TRUE.equals(isRefExist && !refModel.windowSize().isEmpty() && forceResizeWindow) &&
                !shotModel.windowSize().equals(refModel.windowSize())) {
            // Don't know why, but I need to -1 from width
            driver.manage().window().setSize(new Dimension(refModel.width() - 1, refModel.height()));

            shotModel.width(refModel.width())
                    .height(refModel.height());
        } else if (Boolean.TRUE.equals(forceResizeWindow && isRefExist && refModel.windowSize().isEmpty())) {
            var refFullPath = refModel.fullPath();
            logger.warn("Can not resize window, can not retrieve size from this Ref image <{}>", refFullPath);
        }

        if (element == null)
            ShotTacker.takeFullShot(driver, shotModel);
        else
            ShotTacker.takeElementShot(shotModel, element);

        if (Boolean.FALSE.equals(isRefExist)) {
            refModel.width(windowSize.width)
                    .height(windowSize.height);
            saveRefImageAndThrow();
        }

        return new ShotValidation(shotModel, refModel, doNotResize);
    }

    private void prepareScreen() {
        var tempElementList = new ArrayList<>(elements);
        for (By elementsLocator : elementsLocators) {
            tempElementList.add(driver.findElement(elementsLocator));
        }

        var jsExecutor = (JavascriptExecutor) driver;
        tempElementList.forEach(element -> jsExecutor
                        .executeScript("arguments[0].setAttribute('style', 'visibility: hidden')", element));
    }

    private String prepareName() {
        var skipNum = (supportFluent || ConfigHandler.supportFluent()) ? 3 : 2;

        var walker = StackWalker.getInstance();
        var frame = walker.walk(frames -> frames.skip(skipNum).findFirst().orElse(null));

        String name = "";
        if (frame != null)
            name = frame.getClassName() + "#" + frame.getMethodName();

        return name;
    }

    private String prepareName(String name) {
        var extension = Helper.fileExtension(name);

        if (extension != null) {
            name = name.replace(extension, "");
            shotModel.extension(extension);
        }

        return name;
    }

    private String prepareName(String className, String testName) {
        return className + "#" + testName;
    }

    private void setRefModelData(String name) {
        if (Boolean.FALSE.equals(safeRef))
            refModel.path(refPath)
                    .name(name)
                    .timestamp(REF_IMAGE_STAMP);
    }

    private void setShotModelData(String name) {
        shotModel.path(shotPath)
                .name(name)
                .timestamp(timestamp());
    }

    private Boolean loadRefData() {
        var fullPath = getFileFullPathWithPrefix(
                ConfigHandler.refPath(), refModel.name() + NAME_SPLITTER + refModel.timestamp()
        );

        if (!fullPath.isEmpty()) {
            refModel.fullPath(fullPath);
            return true;
        }
        return false;
    }

    private void saveRefImageAndThrow() {
        Helper.moveAndRenameFile(shotModel.fullPath(), refModel.path(), refModel.fullName());

        throw new AssertionError("No reference image found, " +
                "actual shot has been transferred to be reference");
    }
}
