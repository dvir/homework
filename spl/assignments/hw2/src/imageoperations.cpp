#include "../include/imageoperations.h"
 
void ImageOperations::rgb_to_greyscale(const cv::Mat& src, cv::Mat& dst)
{    
    cv::cvtColor(src,dst,CV_RGB2GRAY);
}
 
 
void ImageOperations::resize(const cv::Mat& src, cv::Mat& dst)
{
    cv::resize(src,dst,dst.size());
}
 
void ImageOperations::copy_paste_image(const cv::Mat& original, cv::Mat& destination, int xLocation)
{
    if(original.size().height > destination.size().height)         
        throw ("original image is higher that destination image");
    cv::Rect roi(xLocation, 0, original.size().width, original.size().height);
    cv::Mat imageROI (destination, roi);
    original.copyTo(imageROI);
}
