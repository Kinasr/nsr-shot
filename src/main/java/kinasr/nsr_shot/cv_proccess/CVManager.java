package kinasr.nsr_shot.cv_proccess;

import kinasr.nsr_shot.model.SimilarityTechniques;
import org.bytedeco.javacpp.indexer.Indexer;
import org.bytedeco.javacpp.indexer.UByteIndexer;
import org.bytedeco.opencv.global.opencv_core;
import org.bytedeco.opencv.global.opencv_imgproc;
import org.bytedeco.opencv.opencv_core.Mat;
import org.bytedeco.opencv.opencv_core.Size;

import static org.bytedeco.opencv.global.opencv_imgcodecs.IMREAD_COLOR;
import static org.bytedeco.opencv.global.opencv_imgcodecs.imdecode;
import static org.bytedeco.opencv.global.opencv_imgproc.resize;

public class CVManager {
    private final Mat img1;
    private final Mat img2;

    public CVManager(byte[] img1, byte[] img2) {
        this.img1 = loadImage(img1);
        this.img2 = loadImage(img2);
    }

    /**
     * Returns the difference between two images using the specified similarity technique.
     *
     * @param technique the similarity technique to use (SSI, MSE, MAE, PSNR)
     * @return the difference between the two images
     */
    public double getDiff(SimilarityTechniques technique) {
        return switch (technique) {
            case SSI -> calculateStructuralSimilarityIndex(img1, img2);
            case MSE -> calculateMeanSquaredError(img1, img2);
            case MAE -> calculateMeanAbsoluteError(img1, img2);
            case PSNR -> calculatePSNR(
                    calculateMeanSquaredError(img1, img2)
            );
        };
    }

    /**
     * Resizes img2 to match the size of img1.
     */
    public void resizeImg2ToMatchImg1() {
        resize(img2, img2, img1.size());
    }

    /**
     * Checks if the two images do not have the same size.
     *
     * @return true if the two images do not have the same size, false otherwise
     */
    public boolean isNotTheTwoImagesHaveTheSameSize() {
        var img1Size = img1.size();
        var img2Size = img2.size();
        return img1Size.width() != img2Size.width() || img1Size.height() != img2Size.height();
    }

    /**
     * Returns the size of image1.
     *
     * @return the size of image1
     */
    public Size image1Size() {
        return img1.size();
    }


    public Size image2Size() {
        return img2.size();
    }

    /**
     * Closes the resources used by the function.
     */
    public void close() {
        img1.close();
        img2.close();
    }

    /**
     * Loads an image from a byte array.
     *
     * @param img the byte array containing the image data
     * @return the loaded image as a Mat object
     */
    private Mat loadImage(byte[] img) {
        return imdecode(new Mat(img), IMREAD_COLOR);
    }

    /**
     * Calculates the Structural Similarity Index (SSI) between two images.
     *
     * @param image1 The first image.
     * @param image2 The second image.
     * @return The SSI value as a double.
     */
    private double calculateStructuralSimilarityIndex(Mat image1, Mat image2) {
        Mat ssiResult = new Mat();
        opencv_imgproc.matchTemplate(image1, image2, ssiResult, opencv_imgproc.TM_CCOEFF_NORMED);

        try (Indexer indexer = ssiResult.createIndexer()) {
            return indexer.getDouble();
        }
    }

    /**
     * Calculates the mean squared error between two images.
     *
     * @param image1 the first image
     * @param image2 the second image
     * @return the mean squared error between the two images
     */
    private double calculateMeanSquaredError(Mat image1, Mat image2) {
        Mat mseResult = new Mat();
        opencv_core.absdiff(image1, image2, mseResult);
        UByteIndexer indexer = mseResult.createIndexer();
        double mse = 0;
        for (int y = 0; y < mseResult.rows(); y++) {
            for (int x = 0; x < mseResult.cols(); x++) {
                mse += Math.pow(indexer.get(y, x), 2);
            }
        }
        mse /= (image1.rows() * image1.cols());

        indexer.close();
        return mse;
    }

    /**
     * Calculates the mean absolute error between two images.
     *
     * @param image1 the first image
     * @param image2 the second image
     * @return the mean absolute error between the two images
     */
    private double calculateMeanAbsoluteError(Mat image1, Mat image2) {
        Mat mseResult = new Mat();
        opencv_core.absdiff(image1, image2, mseResult);
        return opencv_core.sumElems(mseResult).get() / (image1.rows() * image1.cols());
    }

    /**
     * Calculates the Peak Signal-to-Noise Ratio (PSNR).
     *
     * @param mse the mean squared error
     * @return the PSNR value
     */
    private double calculatePSNR(double mse) {
        var maxValue = 255;
        return 10 * Math.log10((maxValue * maxValue) / mse);
    }
}
