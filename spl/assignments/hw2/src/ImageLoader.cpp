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
    //destroyAllWindows();  // TODO This shouldn't be here.
}


void ImageLoader :: displayImage(const string department) {

    string windowName;

    if (department.compare(_CS_) == 0) {
        windowName = "Computer Science - Graduation Image";
    } else {
        windowName = "Politics and Government - Graduation Image";
    }

    namedWindow(windowName);  // Create image window named "My image".
    imshow(windowName, m_image);  // Show the image on window.
    waitKey(2000);  // Wait key for 5000 ms

    destroyWindow(windowName);  // Clean up.
}


cv::Mat& ImageLoader :: getImage() {
    return m_image;
}


void ImageLoader :: saveImage(const string& fileName) {
    imwrite(fileName, m_image);
}
