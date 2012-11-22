#include "ImageOperations.h"


using namespace cv;


void ImageOperations :: rgbToGreyscale(const Mat &src, Mat &dst) {
    cvtColor(src, dst, CV_RGB2GRAY);
}


void ImageOperations :: resize(const Mat &src, Mat &dst) {
    cv::resize(src,dst,dst.size());
}


void ImageOperations :: copyPasteImage(
        const Mat &original, Mat &destination, int xLocation) {

    if (original.size().height > destination.size().height) {
        throw ("Error: Original image is higher that destination image.");
    }

    Rect roi(xLocation, 0, original.size().width, original.size().height);
    Mat imageROI (destination, roi);
    original.copyTo(imageROI);
}
