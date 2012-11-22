#ifndef IMAGE_LOADER_H
#define IMAGE_LOADER_H


#include <opencv/highgui.h>
#include <string>
#include <iostream>


class ImageLoader {
    public:
        ImageLoader(int width, int height);
        ImageLoader(const std::string &fileName);
        void displayImage();  // Display an image on screen.
        cv::Mat& getImage();
        void saveImage(const std::string& filename);

        virtual ~ImageLoader();

    private:
        cv::Mat m_image;
};

#endif
