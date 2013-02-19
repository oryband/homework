#ifndef IMAGE_LOADER_H
#define IMAGE_LOADER_H


#include <iostream>
#include <string>

#include <opencv/highgui.h>

#include "consts.h"


class ImageLoader {
    private:
        cv::Mat m_image;

    public:
        // Create a new image with the size = width * height.
        ImageLoader(int width, int height);

        // Import an image from a file location.
        ImageLoader(const std::string& fileName);

        // Display an image on screen.
        void displayImage(const std::string department);

        // Matrix getter.
        cv::Mat& getImage();

        // Save image to filename.
        void saveImage(const std::string& filename);

        virtual ~ImageLoader();
};


#endif
