package kinasr.nsr_shot.shot_manager;

import kinasr.nsr_shot.exception.ShotFileException;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.TakesScreenshot;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.interactions.Actions;

import java.io.FileOutputStream;
import java.io.IOException;

public class ShotTacker {

    private ShotTacker() {}

    public static void takeFullShot(WebDriver driver, String screenshotPath) {
        // Move mouse to the index 0, 0
        moveMouseToPageStart(driver);

        // Take a screenshot of the entire web page
        var screenshot = ((TakesScreenshot) driver).getScreenshotAs(OutputType.BYTES);

        // Save the screenshot to a file
        saveShot(screenshot, screenshotPath);
    }

    public static void takeElementShot(WebDriver driver, String screenshotPath, WebElement element) {
        moveMouseToPageStart(driver);
        var screenshot = element.getScreenshotAs(OutputType.BYTES);

        // Save the screenshot to a file
        saveShot(screenshot, screenshotPath);
    }

    private static void moveMouseToPageStart(WebDriver driver) {
        new Actions(driver)
                .moveToLocation(0, 0)
                .perform();
    }

    private static void saveShot(byte[] screenshot, String screenshotPath) {
        // Save the screenshot to a file
        try (FileOutputStream screenshotOutputStream = new FileOutputStream(screenshotPath)) {
            screenshotOutputStream.write(screenshot);
        } catch (IOException e) {
            throw new ShotFileException("Can not save this screenshot <" + screenshotPath + ">", e);
        }
    }
}
