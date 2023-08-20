package kinasr.nsr_shot.utility;

import kinasr.nsr_shot.exception.ShotFileException;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
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

    public static void moveAndRenameFile(String sourcePath, String destPath) {
        try {
            Files.move(Paths.get(sourcePath), Paths.get(destPath));
        } catch (IOException e) {
            throw new ShotFileException("Can not rename and move <" + sourcePath + ">", e);
        }
    }
}
