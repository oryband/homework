#include "ImageLoader.h"


using namespace cv;
using namespace std;


ImageLoader :: ImageLoader(int width, int height) :
    m_image(width, height, CV_8UC3) {}


ImageLoader :: ImageLoader(const string& fileName) :
    m_image(imread(fileName)) {

    if (!m_image.data) {
        cout << "Failed loading " << fileName << endl;
    }
}


ImageLoader :: ~ImageLoader() {
    m_image.release();
}


void ImageLoader :: displayImage() {
    namedWindow("My image", CV_WINDOW_AUTOSIZE);  // Create image window named "My image".
    imshow("My image", m_image);  // Show the image on window.
    waitKey(2000);  // Wait key for 5000 ms

    // FIXME NEED TO DELETE TEMP IMAGE!!!!!!!
}


cv::Mat& ImageLoader :: getImage() {
    return m_image;
}


void ImageLoader :: saveImage(const string& fileName) {
    imwrite(fileName, m_image);
}
