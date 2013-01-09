#include "../include/inputwindow.h"

class InputWindow : public Window {
    public:
        InputWindow(std::string name, 
               int height, 
               int width, 
               int starty, 
               int startx) :
            Window(name, height, width, starty, startx),
            _inputY(1),
            _inputX(1),
            _input()
        {
        }

        virtual void redraw() {
            this->clear();
            this->print(_inputY, _inputX, this->getInput());
        };

        virtual void setInputY(int inputY) {
            _inputY = inputY;
        };

        virtual void setInputX(int inputX) {
            _inputX = inputX;
        };

        virtual void setInputYX(int inputY, int inputX) {
            _inputY = inputY;
            _inputX = inputX;
        };

        virtual int getChar() {
            return mvwgetch(_win, _inputY, _inputX + _input.str().size());
        };

        virtual void putChar(char ch) {
            _input << (char)ch;
            this->redraw();
        };

        virtual void deleteLastChar() {
            std::string trimedStr = _input.str().substr(0, _input.str().size()-1);
            _input.str("");
            _input << trimedStr;
            this->redraw();
        };
        
        virtual void clearInput() {
            _input.str("");
            _input.clear();
            this->clear();
        };

        virtual std::string getInput() {
            return _input.str();
        };

        virtual std::string str() {
            return _input.str();
        };

    protected:
        int _inputY;
        int _inputX;

        ostringstream _input;
};
