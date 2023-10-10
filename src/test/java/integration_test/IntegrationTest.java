package integration_test;

import kinasr.nsr_shot.Shot;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.chrome.ChromeOptions;

public class IntegrationTest {
    private WebDriver driver;

    // region Locators
    private final By dialogLogin = By.className("login_wrapper-inner");
    private final By dialogLoginCredentials = By.className("login_credentials_wrap-inner");
    private final By wrapperLogin = By.className("login_wrapper");
    // endregion

    @BeforeEach
    void setup() {
        driver = new ChromeDriver(
                new ChromeOptions().addArguments("--remote-allow-origins=*")
        );
        driver.get("https://www.saucedemo.com/");
    }

    @AfterEach
    void tearDown() {
        driver.quit();
    }

    @Test
    void checkThatPageIsVisuallyCorrect() {
        new Shot(driver)
                .takeShot()
                .assertThatShotMatchReference();
    }

    @Test
    void checkThatPageIsVisuallyCorrect2() throws InterruptedException {
        Thread.sleep(1000);
       var r = new Shot(driver)
                .takeShot2()
                .perform();

       Assertions.assertTrue(r.isMatch());
    }

    @Test
    void checkLoginDialogIsVisuallyCorrect() {
        new Shot(driver)
                .takeShot(dialogLogin)
                .assertThatShotMatchReference();
    }

    @Test
    void checkThatPageIsVisuallyCorrectWithoutLoginCredentials() {
        new Shot(driver)
                .ignoreElement(dialogLoginCredentials)
                .takeShot()
                .assertThatShotMatchReference();
    }

    @Test
    void checkThatWrapperLoginIsVisuallyCorrectWithoutLoginCredentials() {
        new Shot(driver)
                .ignoreElement(dialogLoginCredentials)
                .takeShot(wrapperLogin)
                .assertThatShotMatchReference();
    }

    @Test
    void checkThatCanTakeRefAndShotInSameTest() {
        var shot = new Shot(driver)
                .takeRef(wrapperLogin);
        driver.navigate().refresh();

        shot.takeShot(wrapperLogin)
                .assertThatShotMatchReference();
    }

    @Test
    void checkThatShotResultsHasSomeResults() {
        var result = new Shot(driver)
                .takeShot()
                .isMatch();

        Assertions.assertTrue(result.isMatch());
        Assertions.assertTrue(result.ref().image().length > 0);
        Assertions.assertTrue(result.matchedShot().image().length >  0);
    }
}