package kinasr.nsr_shot.exception;

public class ShotFileException extends RuntimeException{

    public ShotFileException(String message) {
        super(message);
    }

    public ShotFileException(String message, Throwable cause) {
        super(message, cause);
    }
}
