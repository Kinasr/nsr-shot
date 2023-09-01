package kinasr.nsr_shot.cv_proccess;

import kinasr.nsr_shot.model.SimilarityTechniques;
import org.bytedeco.javacpp.indexer.Indexer;
import org.bytedeco.javacpp.indexer.UByteIndexer;
import org.bytedeco.opencv.global.opencv_core;
import org.bytedeco.opencv.global.opencv_imgproc;
import org.bytedeco.opencv.opencv_core.Mat;
import org.bytedeco.opencv.opencv_core.Size;

import static org.bytedeco.opencv.global.opencv_imgcodecs.imread;
import static org.bytedeco.opencv.global.opencv_imgproc.resize;

public class CVManager {
    private final Mat img1;
    private final Mat img2;

    public CVManager(String img1Path, String img2Path) {
        this.img1 = loadImage(img1Path);
        this.img2 = loadImage(img2Path);
    }

    public double getDiff(SimilarityTechniques technique) {
        return switch (technique) {
            case SSI -> calculateStructuralSimilarityIndex(img1, img2);
            case MSE -> calculateMeanSquaredError(img1, img2);
            case MAE -> calculateMeanAbsoluteError(img1, img2);
            case PSNR -> calculatePSNR(
                    calculateMeanSquaredError(img1, img2), 255
            );
        };
    }

    public void resizeImg2ToMatchImg1() {
        resize(img2, img2, img1.size());
    }

    private Mat loadImage(String imgPath) {
        return imread(imgPath);
    }

    private double calculateStructuralSimilarityIndex(Mat image1, Mat image2) {
        Mat ssiResult = new Mat();
        opencv_imgproc.matchTemplate(image1, image2, ssiResult, opencv_imgproc.TM_CCOEFF_NORMED);

        try(Indexer indexer= ssiResult.createIndexer()) {
            return indexer.getDouble();
        }
    }

    public boolean isTheTwoImagesHaveTheSameSize() {
        var img1Size = img1.size();
        var img2Size = img2.size();
        return img1Size.width() == img2Size.width() && img1Size.height() == img2Size.width();
    }

    public Size image1Size() {
        return img1.size();
    }

    public Size image2Size() {
        return img2.size();
    }

    public void close() {
        img1.close();
        img2.close();
    }

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

    private double calculateMeanAbsoluteError(Mat image1, Mat image2) {
        Mat mseResult = new Mat();
        opencv_core.absdiff(image1, image2, mseResult);
        return opencv_core.sumElems(mseResult).get() / (image1.rows() * image1.cols());
    }

    private double calculatePSNR(double mse, double maxValue) {
        return 10 * Math.log10((maxValue * maxValue) / mse);
    }
}
