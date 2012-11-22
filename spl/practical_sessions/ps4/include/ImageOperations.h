#ifndef IMAGE_OPERATIONS_H
#define IMAGE_OPERATIONS_H


#include <opencv/highgui.h>
#include <opencv/cv.h>


class ImageOperations {
    public:
        void rgbToGreyscale(const cv::Mat& src, cv::Mat& dst);

        // Copy at location (xLocation, 0).
        void copyPasteImage(const cv::Mat& src, cv::Mat& dst, int xLocation);

        // Resize original picture into the dimension of image destination.
        void resize(const cv::Mat &src, cv::Mat &dst);
};

#endif
