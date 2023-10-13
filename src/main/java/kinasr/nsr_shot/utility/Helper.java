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
import java.util.function.BooleanSupplier;

import static kinasr.nsr_shot.utility.Constant.*;

public class Helper {
    private Helper() {
    }

    public static String timestamp() {
        return String.valueOf(Calendar.getInstance().getTimeInMillis());
    }

    public static String prepareShotName(int depth) {
        var walker = StackWalker.getInstance();
        var frame = walker.walk(frames -> frames.skip(depth).findFirst().orElse(null));

        String name = "";
        if (frame != null)
            name = frame.getClassName() + "#" + frame.getMethodName();

        return name;
    }

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

    public static String getFileFullPathWithPrefix(String directoryPath, String prefix) {
        var matchingFileFullPath = "";

        File directory = new File(directoryPath);
        if (directory.exists()) {
            var files = Arrays.stream(Objects.requireNonNull(directory.listFiles()))
                    .filter(f -> f.getName().startsWith(prefix)).toList();

            if (!files.isEmpty()) {
                if (files.size() > 1)
                    throw new ShotFileException("There are multi files match <" + directoryPath + prefix + ">");

                var file = files.get(0);
                if (file.isFile() && file.getName().startsWith(prefix))
                    matchingFileFullPath = file.getAbsolutePath();
            }
        }

        return matchingFileFullPath;
    }

    public static boolean isDirectoryExist(String path) {
        var file = new File(path);

        return file.exists() && file.isDirectory();
    }

    public static void createDirectory(String path) {
        try {
            Files.createDirectories(Paths.get(path));
        } catch (IOException e) {
            throw new ShotFileException("Can not create directory <" + path + ">", e);
        }
    }

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

    public static void hideUnwantedElements(WebDriver driver, List<By> locators, List<WebElement> elements) {
        for (By locator : locators) {
            var e = driver.findElements(locator);
            if (e.isEmpty())
                throw new NoSuchElementException("Can not find this element <" + locator + ">");

            elements.addAll(e);
        }

        var jsExecutor = (JavascriptExecutor) driver;
        elements.forEach(element -> jsExecutor
                .executeScript("arguments[0].setAttribute('style', 'visibility: hidden')", element));
    }

    /**
     * Repeatedly calls the provided function until it returns true, or the maximum number of repeats is reached.
     *
     * @param repeat   The maximum number of repeats.
     * @param interval The interval in milliseconds between each repeat.
     * @param function The function to be called repeatedly.
     * @return True if the function returns true within the specified repeats, false otherwise.
     */
    public static boolean await(int repeat, long interval, BooleanSupplier function) {
        // Repeat the loop until the maximum number of repeats is reached
        do {
            // Check if the function returns true
            if (function.getAsBoolean())
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
}
