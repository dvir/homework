#include "../include/imageloader.h"
#include <iostream>
 
using namespace cv;
using namespace std;
 
ImageLoader::ImageLoader(int width, int height)
    : m_image(width, height, CV_8UC3)
{
    
}
 
ImageLoader::ImageLoader(const string& fileName)
    : m_image(imread(fileName))
{
  if (!m_image.data)
  {
    cout << "Failed loading " << fileName << endl;
  }
}
 
ImageLoader::~ImageLoader()
{
  m_image.release();
}
 
void ImageLoader::displayImage()
{
    // create image window named "My image"
        namedWindow("My image");
        // show the image on window
        imshow("My image", m_image);
        // wait key for 5000 ms
        waitKey(5000);
}
 
cv::Mat& ImageLoader::getImage()
{
    return m_image;
}
 
void ImageLoader::saveImage(const string& fileName){
    imwrite(fileName, m_image);
}
