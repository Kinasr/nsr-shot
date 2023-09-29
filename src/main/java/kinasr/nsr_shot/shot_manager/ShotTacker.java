package kinasr.nsr_shot.shot_manager;

import kinasr.nsr_shot.exception.ShotFileException;
import kinasr.nsr_shot.model.ShotModel;
import kinasr.nsr_shot.utility.Helper;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.TakesScreenshot;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;

import java.io.FileOutputStream;
import java.io.IOException;

public class ShotTacker {
    private ShotTacker() {
    }

    public static byte[] takeFullShot(WebDriver driver, ShotModel model) {
        // Take a screenshot of the entire web page
        var screenshot = ((TakesScreenshot) driver).getScreenshotAs(OutputType.BYTES);

        // Save the screenshot to a file
        saveShot(screenshot, model);

        return screenshot;
    }

    public static byte[] takeElementShot(ShotModel model, WebElement element) {
        var screenshot = element.getScreenshotAs(OutputType.BYTES);

        // Save the screenshot to a file
        saveShot(screenshot, model);

        return screenshot;
    }

    private static void saveShot(byte[] screenshot, ShotModel model) {
        // Save the screenshot to a file
        try (FileOutputStream screenshotOutputStream = new FileOutputStream(model.fullPath())) {
            screenshotOutputStream.write(screenshot);
        } catch (IOException e) {
            if (Boolean.FALSE.equals(model.doesDirectoryCreated())) {
                model.doesDirectoryCreated(true);

                Helper.createDirectory(model.path());
                saveShot(screenshot, model);
            } else
                throw new ShotFileException("Can not save this screenshot <" + model.fullPath() + ">", e);
        }
    }
}
