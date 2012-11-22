#include "ImageLoader.h"


using namespace cv;
using namespace std;


ImageLoader :: ImageLoader(int width, int height)
    : m_image(width, height, CV_8UC3) {}


ImageLoader :: ImageLoader(const string& fileName)
    : m_image(imread(fileName)) {

        if (!m_image.data) {
            cout << "Failed loading " << fileName << endl;
        }
    }


ImageLoader :: ~ImageLoader() {
    m_image.release();
}


void ImageLoader :: displayImage() {
    namedWindow("My image");
    imshow("My image", m_image);
    waitKey(5000);  // Wait key for 5000 ms
}


Mat& ImageLoader :: getImage() {
    return m_image;
}


void ImageLoader :: saveImage(const string& fileName) {
    imwrite(fileName, m_image);
}
