package kinasr.nsr_shot.shot_manager;

import kinasr.nsr_shot.exception.ShotFileException;
import kinasr.nsr_shot.utility.Helper;
import kinasr.nsr_shot.utility.config.ConfigHandler;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.TakesScreenshot;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;

import java.io.FileOutputStream;
import java.io.IOException;

public class ShotTacker {
    private static boolean isDirectoryCreated = false;

    private ShotTacker() {
    }

    public static void takeFullShot(WebDriver driver, String screenshotPath) {
        // Take a screenshot of the entire web page
        var screenshot = ((TakesScreenshot) driver).getScreenshotAs(OutputType.BYTES);

        // Save the screenshot to a file
        saveShot(screenshot, screenshotPath);
    }

    public static void takeElementShot(String screenshotPath, WebElement element) {
        var screenshot = element.getScreenshotAs(OutputType.BYTES);

        // Save the screenshot to a file
        saveShot(screenshot, screenshotPath);
    }

    private static void saveShot(byte[] screenshot, String screenshotPath) {
        // Save the screenshot to a file
        try (FileOutputStream screenshotOutputStream = new FileOutputStream(screenshotPath)) {
            screenshotOutputStream.write(screenshot);
        } catch (IOException e) {
            if (!isDirectoryCreated) {
                isDirectoryCreated = true;

                Helper.createDirectory(ConfigHandler.shotPath());
                saveShot(screenshot, screenshotPath);
            } else
                throw new ShotFileException("Can not save this screenshot <" + screenshotPath + ">", e);
        }
    }
}
