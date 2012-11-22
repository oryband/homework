#include "ImageLoader.h"
#include "ImageOperations.h"
 
int main(int argc, char **argv)
{
        ImageLoader img1("Lenna.png");
        img1.displayImage();
 
        ImageOperations opr;
 
        ImageLoader img2(100,100);
        opr.resize(img1.getImage(),img2.getImage());
        img2.displayImage();
 
        ImageLoader img3(img1.getImage().size().height, img1.getImage().size().width * 2);
        opr.copy_paste_image(img1.getImage(),img3.getImage(),0);
        opr.copy_paste_image(img1.getImage(),img3.getImage(),img1.getImage().size().width);
        img3.displayImage();
}
