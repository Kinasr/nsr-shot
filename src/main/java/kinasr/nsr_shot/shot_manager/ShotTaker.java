package kinasr.nsr_shot.shot_manager;

import kinasr.nsr_shot.exception.ShotFileException;
import kinasr.nsr_shot.model.ScreenshotModel;
import kinasr.nsr_shot.utility.Helper;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.TakesScreenshot;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;

import java.io.FileOutputStream;
import java.io.IOException;

public class ShotTaker {
    private ShotTaker() {
    }

    public static byte[] takeFullShot(WebDriver driver) {
        return  ((TakesScreenshot) driver).getScreenshotAs(OutputType.BYTES);
    }

    public static byte[] takeElementShot(WebElement element) {
        return element.getScreenshotAs(OutputType.BYTES);
    }
}
