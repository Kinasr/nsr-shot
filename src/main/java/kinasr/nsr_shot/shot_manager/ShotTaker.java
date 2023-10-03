package kinasr.nsr_shot.shot_manager;

import org.openqa.selenium.OutputType;
import org.openqa.selenium.TakesScreenshot;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;

public class ShotTaker {
    private ShotTaker() {
    }

    public static byte[] takeFullShot(WebDriver driver) {
        return ((TakesScreenshot) driver).getScreenshotAs(OutputType.BYTES);
    }

    public static byte[] takeElementShot(WebElement element) {
        return element.getScreenshotAs(OutputType.BYTES);
    }
}
