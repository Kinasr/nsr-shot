package kinasr.nsr_shot;

import kinasr.nsr_shot.model.ShotModel;
import kinasr.nsr_shot.shot_manager.ShotTacker;
import kinasr.nsr_shot.shot_manager.ShotValidation;
import kinasr.nsr_shot.utility.config.ConfigHandler;
import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;

import static kinasr.nsr_shot.utility.Helper.timestamp;

public class Shot {
    private final WebDriver driver;
    private final ShotModel shotModel;


    public Shot(WebDriver driver) {
        this.driver = driver;
        this.shotModel = new ShotModel();
    }

    public Shot ignoreSize() {
        shotModel.ignoreSize(true);
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

    public ShotValidation takeShotWithPath(String path, String name) {
        shotModel.actualShotPath(path)
                .imageName(name);

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

    public ShotValidation takeShotWithPath(WebElement element, String path, String name) {
        shotModel.actualShotPath(path)
                .imageName(name);

        return screenshot(element);
    }

    public ShotValidation takeShotWithPath(By by, String path, String name) {
        shotModel.actualShotPath(path)
                .imageName(name);

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

        if (element == null)
            ShotTacker.takeFullShot(driver, shotModel.actualShotPath());
        else
            ShotTacker.takeElementShot(driver, shotModel.actualShotPath(), element);

        return new ShotValidation(shotModel);
    }

    private void prepareScreen() {
        var jsExecutor = (JavascriptExecutor) driver;

        shotModel.elements()
                .forEach(element -> jsExecutor
                        .executeScript("arguments[0].setAttribute('style', 'visibility: hidden')", element));
    }

    private void setNameAndPath() {
        var walker = StackWalker.getInstance();
        var frame = walker.walk(frames -> frames.skip(2).findFirst().orElse(null));

        String name = "";
        if (frame != null)
            name = frame.getClassName() + "#" + frame.getMethodName() + "_";

        var path = ConfigHandler.actualPath()
                + name + timestamp() + ".png";
        shotModel.actualShotPath(path)
                .imageName(name);
    }

    private void setNameAndPath(String name) {
        var path = ConfigHandler.actualPath()
                + name + ".png";
        shotModel.actualShotPath(path)
                .imageName(name);
    }

    private void setNameAndPath(String className, String testName) {
        var name = className + "#" + testName + "_";
        var path = ConfigHandler.actualPath()
                + name + timestamp() + ".png";
        shotModel.actualShotPath(path)
                .imageName(name);
    }
}
