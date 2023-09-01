package kinasr.nsr_shot.utility;

import kinasr.nsr_shot.exception.ShotFileException;
import kinasr.nsr_shot.model.ShotModel;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Calendar;

import static kinasr.nsr_shot.utility.Constant.*;

public class Helper {
    private Helper() {
    }

    public static String timestamp() {
        return String.valueOf(Calendar.getInstance().getTimeInMillis());
    }

    public static boolean isDirectoryExist(String path) {
        var file = new File(path);

        return file.exists() && file.isDirectory();
    }

    public static void moveAndRenameFile(String sourceFullPath, String destPath, String destName) {
        try {
            if (!isDirectoryExist(destPath)) {
                createDirectory(destPath);
            }
            Files.move(Paths.get(sourceFullPath), Paths.get(destPath, destName));
        } catch (IOException e) {
            throw new ShotFileException("Can not rename and move <" + sourceFullPath + ">", e);
        }
    }

    public static void createDirectory(String path) {
        try {
            Files.createDirectories(Paths.get(path));
        } catch (IOException e) {
            throw new ShotFileException("Can not create directory <" + path + ">", e);
        }
    }

    public static String fileExtension(String fileName) {
        var lastIndexOfPeriod = fileName.lastIndexOf(FULL_STOP);

        if (lastIndexOfPeriod != -1)
            return fileName.substring(lastIndexOfPeriod);

        return null;
    }

    public static ShotModel separateFullPath(String fullPath) {
        var shot = new ShotModel();
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

    public static ShotModel separateFullName(String fullName) {
        var shot = new ShotModel();

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
        File[] files = directory.listFiles();

        if (files != null) {
            if (files.length > 1)
                throw new ShotFileException("There are multi files match <" + directoryPath + prefix + ">");

            else if (files.length == 1) {
                var file = files[0];
                if (file.isFile() && file.getName().startsWith(prefix))
                    matchingFileFullPath = file.getAbsolutePath();
            }
        }

        return matchingFileFullPath;
    }
}
