#include "../include/window.h"

class Window {
    public:
        Window(std::string name, 
               int height, 
               int width, 
               int starty, 
               int startx) :
            _win(newwin(height, width, starty, startx)),
            _name(name),
            _height(height),
            _width(width),
            _starty(starty),
            _startx(startx),
            _refreshAfter()
        {
            // allow detections of special keys
            keypad(_win, TRUE);

            this->setup();

            this->refreshWindow();
        };

	Window::Window (const Window& other) :
            _win(newwin(other.height, other.width, other.starty, other.startx)),
            _name(other.name),
            _height(other.height),
            _width(other.width),
            _starty(other.starty),
            _startx(other.startx),
            _refreshAfter()
	{
	}

	Window& Window::operator=(const Window& other) {
	    if (this == &other) {
	        return *this;
	    }

	    this->_win = other._win;
	    this->_name = other._name;
	    this->_height = other._height;
	    this->_width = other._width;
            this->_starty = other._starty;
	    this->_startx = other._startx;
	    return *this;
	}

        virtual void setup() {
            // draw window borders
            wborder(_win, '|', '|', '-', '-', '+', '+', '+', '+');
        };

        virtual void refreshWindow() {
            wrefresh(_win);
            this->refreshAfter();    
        };

        virtual void print(std::string text) {
            wprintw(_win, text.c_str());
            this->refreshAfter();    
        };

        virtual void print(int y, int x, std::string text) {
            mvwprintw(_win, y, x, text.c_str());
            this->refreshAfter();    
        };

        /**
         * Refresh all windows in _refreshAfter vector.
        **/
        virtual void refreshAfter() {
            for (Windows::iterator it = _refreshAfter.begin();
                 it != _refreshAfter.end(); 
                 ++it)
            {
                (*it)->refreshWindow();
            }
        };

        virtual void addRefreshAfterWindow(Window* window) {
            _refreshAfter.push_back(window);
        };

        virtual void clear() {
            wclear(_win);
            this->setup();
            this->refreshWindow();
        };

        virtual ~Window() {
            delwin(_win);
        };

    protected:
        WINDOW* _win;
        std::string _name;
        int _height;
        int _width;
        int _starty;
        int _startx;
        Windows _refreshAfter;
};
