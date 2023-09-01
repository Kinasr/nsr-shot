package kinasr.nsr_shot;

import kinasr.nsr_shot.model.ShotModel;
import kinasr.nsr_shot.shot_manager.ShotTacker;
import kinasr.nsr_shot.shot_manager.ShotValidation;
import kinasr.nsr_shot.utility.Helper;
import kinasr.nsr_shot.utility.config.ConfigHandler;
import org.openqa.selenium.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static kinasr.nsr_shot.utility.Constant.NAME_SPLITTER;
import static kinasr.nsr_shot.utility.Constant.REF_IMAGE_STAMP;
import static kinasr.nsr_shot.utility.Helper.getFileFullPathWithPrefix;
import static kinasr.nsr_shot.utility.Helper.timestamp;

public class Shot {
    private static final Logger logger = LoggerFactory.getLogger(Shot.class);
    private final WebDriver driver;
    private final ShotModel shotModel = new ShotModel();
    private final ShotModel refModel = new ShotModel();
    private final String shotPath;
    private final String refPath;
    private Boolean ignoreSize = false;
    private Boolean forceResizeWindow = false;
    private Boolean supportFluent = false;

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

    public Shot ignoreSize() {
        this.ignoreSize = true;
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
        var element = driver.findElement(by);
        return ignoreElement(element);
    }

    public Shot ignoreElement(WebElement element) {
        shotModel.addIgnoreElement(element);
        return this;
    }

    public ShotValidation takeShot() {
        setNameAndPath();

        return screenshot(null);
    }

    public ShotValidation takeShot(String name) {
        setNameAndPath(name);

        return screenshot(null);
    }

    public ShotValidation takeShot(String className, String testName) {
        setNameAndPath(className, testName);

        return screenshot(null);
    }

    public ShotValidation takeShot(WebElement element) {
        setNameAndPath();

        return screenshot(element);
    }

    public ShotValidation takeShot(By by) {
        setNameAndPath();

        var element = driver.findElement(by);
        return screenshot(element);
    }

    public ShotValidation takeShot(WebElement element, String name) {
        setNameAndPath(name);

        return screenshot(element);
    }

    public ShotValidation takeShot(By by, String name) {
        setNameAndPath(name);

        var element = driver.findElement(by);
        return screenshot(element);
    }

    public ShotValidation takeShot(WebElement element, String className, String testName) {
        setNameAndPath(className, testName);

        return screenshot(element);
    }

    public ShotValidation takeShot(By by, String className, String testName) {
        setNameAndPath(className, testName);

        var element = driver.findElement(by);
        return screenshot(element);
    }

    private ShotValidation screenshot(WebElement element) {
        prepareScreen();
        var windowSize = driver.manage().window().getSize();
        var isRefExist = loadRefData();

        shotModel.width(windowSize.width)
                .height(windowSize.height);

        if (Boolean.TRUE.equals(isRefExist && !refModel.windowSize().isEmpty() && forceResizeWindow) &&
                !shotModel.windowSize().equals(refModel.windowSize())) {
            // Don't know why, but I need to -1 from width
            driver.manage().window().setSize(new Dimension(refModel.width() - 1, refModel.height()));

            shotModel.width(refModel.width())
                    .height(refModel.height());
        } else if (Boolean.TRUE.equals(forceResizeWindow && refModel.windowSize().isEmpty())) {
            var refFullPath = refModel.fullPath();
            logger.warn("Can not resize window, can not retrieve size from this Ref image <{}>", refFullPath);
        }

        if (element == null)
            ShotTacker.takeFullShot(driver, shotModel.fullPath());
        else
            ShotTacker.takeElementShot(driver, shotModel.fullPath(), element);

        if (Boolean.FALSE.equals(isRefExist)){
            refModel.width(windowSize.width)
                    .height(windowSize.height);
            saveRefImageAndThrow();
        }

        return new ShotValidation(shotModel, refModel, ignoreSize);
    }

    private void prepareScreen() {
        var jsExecutor = (JavascriptExecutor) driver;

        shotModel.elements()
                .forEach(element -> jsExecutor
                        .executeScript("arguments[0].setAttribute('style', 'visibility: hidden')", element));
    }

    private void setNameAndPath() {
        var skipNum = (supportFluent || ConfigHandler.supportFluent()) ? 3 : 2;

        var walker = StackWalker.getInstance();
        var frame = walker.walk(frames -> frames.skip(skipNum).findFirst().orElse(null));

        String name = "";
        if (frame != null)
            name = frame.getClassName() + "#" + frame.getMethodName();

        setModelData(name);
    }

    private void setNameAndPath(String name) {
        var extension = Helper.fileExtension(name);

        if (extension != null) {
            name = name.replace(extension, "");
            shotModel.extension(extension);
        }

        setModelData(name);
    }

    private void setNameAndPath(String className, String testName) {
        setModelData(className + "#" + testName);
    }

    private void setModelData(String name) {
        shotModel.path(shotPath)
                .name(name)
                .timestamp(timestamp());
        refModel.path(refPath)
                .name(name)
                .timestamp(REF_IMAGE_STAMP);
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
