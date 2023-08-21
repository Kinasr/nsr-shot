package kinasr.nsr_shot.utility;

import kinasr.nsr_shot.exception.ShotFileException;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Calendar;

public class Helper {
    private Helper() {
    }

    public static Long timestamp() {
        return Calendar.getInstance().getTimeInMillis();
    }

    public static Boolean isFileExist(String filePath) {
        var file = new File(filePath);

        return file.exists() && !file.isDirectory();
    }

    public static boolean isDirectoryExist(String path) {
        var file = new File(path);

        return file.exists() && file.isDirectory();
    }

    public static void moveAndRenameFile(String sourcePath, String destPath) {
        try {
            if (!isDirectoryExist(destPath)){
                createDirectory(getPathFromFile(destPath));
            }
            Files.move(Paths.get(sourcePath), Paths.get(destPath));
        } catch (IOException e) {
            throw new ShotFileException("Can not rename and move <" + sourcePath + ">", e);
        }
    }

    public static void createDirectory(String path) {
        try {
            Files.createDirectories(Paths.get(path));
        } catch (IOException e) {
            throw new ShotFileException("Can not create directory <" + path + ">", e);
        }
    }

    public static String getPathFromFile(String filePath) {
        var splitter = "";
        if (filePath.contains("/"))
            splitter = "/";
        else if (filePath.contains("\\"))
            splitter = "\\\\";

        var path = new StringBuilder();
        if (!splitter.isEmpty()) {
            var directories = new java.util.ArrayList<>(Arrays.stream(filePath.split(splitter)).toList());
            directories.remove(directories.get(directories.size() - 1));

            for (String directory : directories) {
                path.append(directory)
                        .append(splitter);
            }

            return path.toString();
        }
        return null;
    }
}
