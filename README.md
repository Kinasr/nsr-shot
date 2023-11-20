NSR-SHOT
---
NSR-SHOT is a lightweight library for visual testing built on JavaCV and Selenium. It provides a simple and convenient 
way to compare screenshots of web pages and elements to reference images.

## Quick Start Guide

### 1. Installation
- Learn how to install a Maven package from GitHub by visiting [this link](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#installing-a-package).
- To install NSR-SHOT, you can add the following dependency to your project's pom.xml file:
    ```xml
    <dependency>
        <groupId>io.github.kinasr</groupId>
        <artifactId>nsr-shot</artifactId>
        <version>0.0.7-alpha</version>
    </dependency>
    ```
- You will also need to add the following dependencies for Selenium and NSR-YAML:
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
- Once you have added the dependencies, you can run `mvn install` to install the library.

### 2. Hello World

Here is an example of how to use NSR-SHOT to take a screenshot and compare it to a reference image:
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
- The reference image will be created automatically if not exist.

### 3. Comparing Techniques

NSR-SHOT supports three different image comparison techniques:

* **MSE (Mean Squared Error):** This is a pixel-based metric that calculates the average squared difference between corresponding pixels in two images. The lower the MSE value, the more similar the two images are. However, MSE is sensitive to small changes in pixel values, and it may not accurately reflect the perceived quality of an image for the human visual system.

* **PNSR (Peak Signal-to-Noise Ratio):** This is a more perceptually relevant metric that measures the ratio between the peak signal power of the original image and the power of the noise in the distorted image. It is expressed in decibels (dB), and higher PSNR values indicate better image quality. PSNR is generally considered a more reliable metric for assessing image quality compared to MSE.

* **SSI (Structural Similarity Index):** This is a more sophisticated metric that takes into account the human visual system's sensitivity to structural distortions in images. It measures the similarity of the structural information between two images, such as the arrangement of pixels and the relationship between different regions of the images. SSI values range from 0 to 1, where 1 indicates perfect structural similarity.


### Conclusion
NSR-SHOT is a powerful and easy-to-use library for visual testing. It provides a variety of features that make it a great choice for automating visual tests, including:

* Support for three different image comparison techniques
* Ability to ignore elements when taking screenshots
* Automatic generation of reference images
* Support for multiple browsers and operating systems