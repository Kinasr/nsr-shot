package kinasr.nsr_shot.shot_manager;

import org.openqa.selenium.OutputType;
import org.openqa.selenium.TakesScreenshot;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;

public class ShotTaker {
    private ShotTaker() {
    }
    /**
     * Takes a full screenshot of the current web page.
     *
     * @param driver The WebDriver instance.
     * @return The byte array representation of the screenshot.
     */
    public static byte[] takeFullShot(WebDriver driver) {
        return ((TakesScreenshot) driver).getScreenshotAs(OutputType.BYTES);
    }

    /**
     * Takes a screenshot of a given web element.
     *
     * @param element The web element to capture the screenshot of.
     * @return The screenshot of the web element as a byte array.
     */
    public static byte[] takeElementShot(WebElement element) {
        return element.getScreenshotAs(OutputType.BYTES);
    }
}
