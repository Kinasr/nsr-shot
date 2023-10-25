NSR-SHOT
---
A light-weight library for visual testing build on JavaCV and Selenium

## Quick Start Guide

### 1. Installation
- Learn how to install a Maven package from GitHub by visiting [this link](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#installing-a-package).
- Add this library as a dependency in your project's pom.xml file:
    ```xml
    <dependency>
        <groupId>io.github.kinasr</groupId>
        <artifactId>nsr-shot</artifactId>
        <version>0.0.7-alpha</version>
    </dependency>
    ```
- Provide Selenium and NSR-YAML dependencies
    ```xml
    <dependency>
        <groupId>org.seleniumhq.selenium</groupId>
        <artifactId>selenium-java</artifactId>
        <version>4.11.0</version>
    </dependency>

    <dependency>
        <groupId>io.github.kinasr</groupId>
        <artifactId>nsr-yaml</artifactId>
        <version>0.0.3-beta</version>
    </dependency>
    ```
- Run `mvn install` to ensure the library is installed.

### 2. Hello World

```java
public class HelloWorld {
  public static void main(String[] args) {
    WebDriver driver; // Initialize driver

    // Check that full page screenshot match the reference image
    new Shot(driver)
            .takeShot()
            .verify();

    // Check that screenshot for this element with ignoring element match reference image 
    new Shot(driver)
            .ignoreElement(By.id("element_need_to_be_hidden"))
            .takeShot(By.id("take_screenshot_for_this_element"))
            .verify();
  }
}
```
- The reference image will be created automatically if not exist