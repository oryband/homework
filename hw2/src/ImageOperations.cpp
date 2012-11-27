#include "ImageOperations.h"


void ImageOperations :: rgb_to_greyscale(
        const cv :: Mat& src, cv :: Mat& dst) {
    cv :: cvtColor(src, dst, CV_RGB2GRAY);
    cv :: cvtColor(dst, dst, CV_GRAY2RGB);  // Greyscale fix.
}


void ImageOperations :: resize(const cv :: Mat& src, cv :: Mat& dst) {
    cv :: resize(src, dst, dst.size());
}


void ImageOperations :: copy_paste_image(
        const cv :: Mat& original, cv :: Mat& destination, int xLocation) {

    if (original.size().height > destination.size().height) {
        throw ("original image is higher that destination image");
    }
    std::cout << " here with a probelm???? " << std::endl;
    cv :: Rect roi(
            xLocation,
            0,
            original.size().width,
            original.size().height);

    std::cout << " 1 here with a probelm???? " << std::endl;
    cv :: Mat imageROI(destination, roi);
    std::cout << " 2 here with a probelm???? " << std::endl;

    original.copyTo(imageROI);

    std::cout << " 3 here with a probelm???? " << std::endl;

}
