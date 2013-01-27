#ifndef WINDOW_H
#define WINDOW_H

#include "../include/typedef.h"
#include <curses.h>

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

	Window(const Window& other);

	Window& operator=(const Window& other);

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

#endif
