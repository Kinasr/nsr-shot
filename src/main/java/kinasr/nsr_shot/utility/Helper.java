package kinasr.nsr_shot.utility;

import kinasr.nsr_shot.exception.ShotFileException;
import kinasr.nsr_shot.model.ScreenshotModel;
import org.openqa.selenium.*;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Calendar;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.BooleanSupplier;
import java.util.function.IntConsumer;

import static kinasr.nsr_shot.utility.Constant.*;

public class Helper {
    private Helper() {
    }

    /**
     * Returns the current timestamp as a string.
     *
     * @return  the current timestamp as a string
     */
    public static String timestamp() {
        return String.valueOf(Calendar.getInstance().getTimeInMillis());
    }

    /**
     * Generates a shot name for a given depth in the stack trace.
     *
     * @param  depth  the depth in the stack trace
     * @return        the generated shot name
     */
    public static String prepareShotName(int depth) {
        var walker = StackWalker.getInstance();
        var frame = walker.walk(frames -> frames.skip(depth).findFirst().orElse(null));

        String name = "";
        if (frame != null)
            name = frame.getClassName() + "#" + frame.getMethodName();

        return name;
    }

    /**
     * Separates the full path into the path and the filename, and returns a ScreenshotModel object.
     *
     * @param  fullPath   the full path of the file
     * @return            a ScreenshotModel object representing the separated path and filename
     */
    public static ScreenshotModel separateFullPath(String fullPath) {
        var shot = new ScreenshotModel();
        var path = "";

        var lastSeparatorIndex = fullPath.lastIndexOf("\\");
        if (lastSeparatorIndex == -1)
            lastSeparatorIndex = fullPath.lastIndexOf("/");

        if (lastSeparatorIndex != -1) {
            path = fullPath.substring(0, lastSeparatorIndex + 1);

            if (lastSeparatorIndex < fullPath.length() - 1)
                shot = separateFullName(fullPath.substring(lastSeparatorIndex + 1));

        } else
            shot = separateFullName(fullPath);

        return shot.path(path);
    }

    /**
     * Separates the components of a full name and creates a ScreenshotModel object.
     *
     * @param  fullName  the full name to be separated
     * @return           the ScreenshotModel object with the separated components
     */
    public static ScreenshotModel separateFullName(String fullName) {
        var shot = new ScreenshotModel();

        var exLastIndex = fullName.lastIndexOf(FULL_STOP);
        if (exLastIndex != -1) {
            shot.extension(fullName.substring(exLastIndex));
            fullName = fullName.replace(shot.extension(), EMPTY_STRING);
        }

        while (true) {
            var sub = fullName.substring(fullName.lastIndexOf(NAME_SPLITTER) + 1);

            if (sub.contains(SIZE_SPLITTER)) {
                shot.windowSize(sub);
            } else if (sub.matches("\\d+") || sub.equals(REF_IMAGE_STAMP)) {
                shot.timestamp(sub);
            } else {
                shot.name(fullName);
                break;
            }
            fullName = fullName.replace(NAME_SPLITTER + sub, EMPTY_STRING);
        }

        return shot;
    }

    /**
     * Retrieves the full path of a file in a given directory that starts with the specified prefix.
     *
     * @param  directoryPath  the path of the directory where the file is located
     * @param  prefix         the prefix that the file name should start with
     * @return                the full path of the matching file, or an empty string if no file matches the prefix
     */
    public static String getFileFullPathWithPrefix(String directoryPath, String prefix) {
        AtomicReference<String> fileFullPath = new AtomicReference<>("");

        File directory = new File(directoryPath);
        if (directory.exists()) {
            var files = Arrays.stream(Objects.requireNonNull(directory.listFiles()))
                    .filter(f -> f.getName().startsWith(prefix)).toList();

            if (files.size() > 1)
                throw new ShotFileException("There are multi files match <" + directoryPath + prefix + ">");

            files.stream().findFirst().ifPresent(f -> fileFullPath.set(f.getAbsolutePath()));
        }
        return fileFullPath.get();
    }

    /**
     * Check if a directory exists at the given path.
     *
     * @param path the path to the directory
     * @return true if the directory exists, false otherwise
     */
    public static boolean isDirectoryExist(String path) {
        var file = new File(path);
        return file.exists() && file.isDirectory();
    }

    /**
     * Creates a directory at the specified path.
     *
     * @param path the path where the directory will be created
     */
    public static void createDirectory(String path) {
        try {
            Files.createDirectories(Paths.get(path));
        } catch (IOException e) {
            throw new ShotFileException("Can not create directory <" + path + ">", e);
        }
    }

    /**
     * Saves a screenshot to a file.
     *
     * @param screenshot The byte array representing the screenshot
     * @param path       The path to the directory where the screenshot will be saved
     * @param fileName   The name of the file to save the screenshot as
     * @throws ShotFileException If there is an error saving the screenshot
     */
    public static void saveShot(byte[] screenshot, String path, String fileName) {
        // Save the screenshot to a file
        try (FileOutputStream screenshotOutputStream = new FileOutputStream(path + fileName)) {
            screenshotOutputStream.write(screenshot);
        } catch (IOException e) {
            if (!isDirectoryExist(path)) {
                createDirectory(path);
                saveShot(screenshot, path, fileName);
            } else
                throw new ShotFileException("Can not save this screenshot <" + path + fileName + ">", e);
        }
    }

    /**
     * Hide unwanted elements using the provided locators and elements.
     *
     * @param driver   The WebDriver instance.
     * @param locators The list of locators to find the elements.
     * @param elements The list of elements to hide.
     * @throws NoSuchElementException If an element cannot be found.
     */
    public static void hideUnwantedElements(WebDriver driver, List<By> locators, List<WebElement> elements) {
        // Find elements using the locator
        for (By locator : locators) {
            var foundElements = driver.findElements(locator);
            if (foundElements.isEmpty())
                throw new NoSuchElementException("Can not find this element <" + locator + ">");

            elements.addAll(foundElements);
        }

        // Hide the elements using JavaScriptExecutor
        var jsExecutor = (JavascriptExecutor) driver;
        elements.forEach(element -> jsExecutor
                .executeScript("arguments[0].setAttribute('style', 'visibility: hidden')", element));
    }

    /**
     * Repeatedly calls the provided function until it returns true, or the maximum number of repeats is reached.
     *
     * @param repeat   The maximum number of repeats.
     * @param interval The interval in milliseconds between each repeat.
     * @param supplier The function to be called repeatedly.
     * @return True if the function returns true within the specified repeats, false otherwise.
     */
    public static boolean await(int repeat, long interval, BooleanSupplier supplier) {
        // Repeat the loop until the maximum number of repeats is reached
        do {
            // Check if the function returns true
            if (supplier.getAsBoolean())
                return true;

            // Sleep for the specified interval if there are more repeats to be done
            if (repeat > 0) {
                try {
                    Thread.sleep(interval);
                } catch (InterruptedException e) {
                    // Ignore the exception and return false
                }
            }
        } while (repeat-- > 0);

        // Return false if the function does not return true within the specified repeats
        return false;
    }

    /**
     * Repeats a consumer function a specified number of times with a given interval.
     *
     * @param repeat   The number of times to repeat the consumer function.
     * @param interval The interval between each repetition in milliseconds.
     * @param consumer The consumer function to be repeated.
     */
    public static void repeat(int repeat, long interval, IntConsumer consumer) {
        for (int i = 1; i < repeat; i++) {
            consumer.accept(i);
            try {
                Thread.sleep(interval);
            } catch (InterruptedException e) {
                // Ignore the exception
            }
        }
    }
}
