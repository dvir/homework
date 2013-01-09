//#include "../include/contentwindow.h"

template <typename T>
class ContentWindow : public Window {
    public:
        ContentWindow(
            std::string name, 
            int height, 
            int width, 
            int starty, 
            int startx
        ) :
            Window(name, height, width, starty, startx),
            _content(""),
            _contentElement(NULL),
            _offsetY(1),
            _offsetX(1)
        {
        }

        virtual void redraw() {
            this->clear();
            
            this->print(_offsetY, _offsetY, this->getContent());
            this->refreshWindow();
        };
        
        virtual void setOffsetY(int offsetY) {
            _offsetY = offsetY;
        };

        virtual void setOffsetX(int offsetX) {
            _offsetX = offsetX;
        };

        virtual void setOffsetYX(int offsetY, int offsetX) {
            _offsetY = offsetY;
            _offsetX = offsetX;
        };

        virtual size_t getOffsetY() const {
            return _offsetY;
        };

        virtual size_t getOffsetX() const {
            return _offsetX;
        };

        virtual void setContent(T t) {
            _contentElement = t;

            this->redraw();
        };

        virtual void setContent(std::string content) {
            _content = content;

            this->redraw();
        };

        virtual void appendContent(std::string content) {
            _content += content;
            
            this->redraw();
        };

        virtual void appendContent(char ch) {
            ostringstream ss;
            ss << (char)ch;

            this->appendContent(ss.str());
        };
        
        virtual std::string getContent() const {
            if (NULL == _contentElement) {
                return _content;
            }

            return _contentElement->toString();
        };
        
        virtual void clearContent() {
            _content.clear();
            this->clear();
        };

    protected:
        std::string _content;
        T _contentElement;
        int _offsetY;
        int _offsetX;
};
