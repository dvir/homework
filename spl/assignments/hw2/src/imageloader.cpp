#include "../include/imageloader.h"
 
ImageLoader::ImageLoader(int width, int height)
    : m_image(width, height, CV_8UC3)
{
}

ImageLoader::ImageLoader(const std::string& fileName)
    : m_image(cv::imread(fileName))
{
  if (!m_image.data)
  {
      throw ("Faiiled loading file.");
  }
}

ImageLoader::~ImageLoader()
{
    m_image.release();
}

void ImageLoader::displayImage()
{
    // create image window named "My image"
//    cv::namedWindow("My image");
    
    // show the image on window
    imshow("My image", m_image);

    // wait key for 5000 ms
    cv::waitKey(2000);

    cv::destroyWindow("My image");
}
 
void ImageLoader::saveImage(const std::string& fileName){
    imwrite(fileName, m_image);
}
