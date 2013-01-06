#include <curses.h>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>
#include <algorithm>
using namespace std;

class User;
class Channel;

typedef vector<std::string> Names;
typedef vector<std::string> History;
typedef vector<User*> Users;
typedef vector<Channel*> Channels;

class User {
    public:
        User(std::string nick, std::string name) :
            _nick(nick),
            _name(name),
            _channels()
        {
            this->_name = name;
        };

        void addChannel(Channel* channel) {
            _channels.push_back(channel);
        };

        void removeChannel(Channel* channel) {
            Channels::iterator position = std::find(
                                                _channels.begin(), 
                                                _channels.end(),
                                                channel
                                                );
            if (position != _channels.end()) {
                _channels.erase(position);
            }
        };
        
        bool isInChannel(Channel* channel) {
            Channels::iterator position = std::find(
                                                _channels.begin(), 
                                                _channels.end(),
                                                channel
                                                );
            return (position != _channels.end());
        };

        void setNick(std::string nick) {
            this->_nick = nick;
        };

        std::string getName() const {
            return this->_name;
        };

        std::string getNick() const {
            return this->_nick;
        };

        std::string toString() const {
            return this->_nick;
        };

    private:
        std::string _nick;
        std::string _name;
        Channels _channels;
};

class Channel {
    public:
        Channel(std::string name) : 
            _name(name), 
            _topic(""), 
            _users() 
        {
        };

        void setTopic(std::string topic) {
            this->_topic = topic;  
        };

        void addUser(User* user) {
            this->_users.push_back(user);
        };

        void removeUser(User* user) {
            Users::iterator position = std::find(
                                                this->_users.begin(), 
                                                this->_users.end(),
                                                user
                                                );
            if (position != _users.end()) {
                this->_users.erase(position);
            }
        };

        size_t getUsersCount() const {
            return this->_users.size();
        };

        std::string getName() const {
            return this->_name;
        };

        std::string getTopic() const {
            return this->_topic;
        };

        Users getUsers() const {
            return this->_users;
        };

    private:
        std::string _name;
        std::string _topic;
        Users _users;
};

class Message {
    public:
        Message(User* user, std::string text) :
            _user(user),
            _text(text),
            _nick(user->getNick())
        {
        };

        Message (Message& other) :
            _user(new User("Copy", "John Copy")),
            _text("copy"),
            _nick("Copy-nick")
        {
        };
        
        Message operator=(Message& other) {
            _user = new User("Copy", "John Copy");
            _text = "copy";
            _nick = "Copy-nick";

            return other;
        };

        const User& getUser() const {
            return *_user;
        };

        std::string getText() const {
            return _text;
        };

        std::string toString() {
            return string()
                    .append("<")
                    .append(this->_nick)
                    .append("> ")
                    .append(this->getText());
        };

    private:
        User* _user;
        std::string _text;
        std::string _nick;
};

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
            _startx(startx)
        {
            // allow detections of special keys
            keypad(_win, TRUE);

            this->setup();

            this->refresh();
        };

        virtual void setup() {
            // draw window borders
            wborder(_win, '|', '|', '-', '-', '+', '+', '+', '+');
        };

        virtual void refresh() {
            wrefresh(_win);
        };

        virtual void print(std::string text) {
            wprintw(_win, text.c_str());
        };

        virtual void print(int y, int x, std::string text) {
            mvwprintw(_win, y, x, text.c_str());
        };

        virtual void clear() {
            wclear(_win);
            this->setup();
            this->refresh();
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
};

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

        void redraw() {
            this->clear();
            print(_inputY, _inputX, getInput());
        };

        void setInputY(int inputY) {
            _inputY = inputY;
        };

        void setInputX(int inputX) {
            _inputX = inputX;
        };

        void setInputYX(int inputY, int inputX) {
            _inputY = inputY;
            _inputX = inputX;
        };

        int getChar() {
            return mvwgetch(_win, _inputY, _inputX + _input.str().size());
        };

        void putChar(char ch) {
            _input << (char)ch;
            this->redraw();
        };

        void deleteLastChar() {
            std::string trimedStr = _input.str().substr(0, _input.str().size()-1);
            _input.str("");
            _input << trimedStr;
            this->redraw();
        };
        
        void clearInput() {
            _input.str("");
            _input.clear();
            this->clear();
        };

        std::string getInput() {
            return _input.str();
        };

        std::string str() {
            return _input.str();
        };
    
    protected:
        int _inputY;
        int _inputX;

        ostringstream _input;
};

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
            _offsetY(1),
            _offsetX(1)
        {
        }

        virtual void redraw() {
            this->clear();
            
            print(_offsetY, _offsetY, this->getContent());
            refresh();
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
            return _content;
        };
        
        virtual void clearContent() {
            _content.clear();
            this->clear();
        };

    protected:
        std::string _content;
        int _offsetY;
        int _offsetX;
};

template <class T>
class ListWindow : public ContentWindow {
    public:
        ListWindow(
            std::string name, 
            int height, 
            int width, 
            int starty, 
            int startx
        ) :
            ContentWindow(name, height, width, starty, startx),
            _list(),
            _visibleSize(-1)
        {
        }

        virtual void redraw() {
            this->clear();
            
            int offset = max(0, (int)_list.size() - _visibleSize);
            if (_visibleSize == -1) {
                offset = 0;
            }

            int i = 0;
            for (typename std::vector<T>::iterator it = _list.begin()
                 + offset;
                 it != _list.end();
                 ++it, ++i)
            {
                print(_offsetY + i, _offsetX, (*it)->toString());
            }

            refresh();
        };

        virtual void addItem(T item) {
            _list.push_back(item);

            this->redraw();
        };

        virtual void setItem(int index, T item) {
            _list.at(index) = item;
            
            this->redraw();
        };

        virtual void removeItem(int index) {
            _list.erase(_list.begin() + index);

            this->redraw();
        };

        virtual void setVisibleSize(size_t size) {
            _visibleSize = size;
        };
        
    protected:
        std::vector<T> _list;
        int _visibleSize;
};

int main(int argc, char *argv[])
{  
    User* user = new User("dvir", "Dvir Azulay");

    Channel channel("chill");
    channel.setTopic("Just hangin' around :)");

    int ch = 0;

    initscr();          /* Start curses mode        */
    start_color();          /* Start the color functionality */
    cbreak();           /* Line buffering disabled, Pass on
                         * everty thing to me       */
    keypad(stdscr, TRUE);       /* I need that nifty F1     */
    noecho();
    init_pair(1, COLOR_GREEN, COLOR_BLACK);
    
//    attron(COLOR_PAIR(1));
//    mvprintw(0, 0, "Press End to exit");
//    refresh();
//    attroff(COLOR_PAIR(1));
    
    ContentWindow wTitle("title", 3, 169, 0, 0);
    ListWindow<Message*> wHistory("main", 44, 153, 2, 0);
    InputWindow input("input", 3, 153, 45, 0);
    ListWindow<User*> wNames("names", 46, 16, 2, 153);

    wNames.addItem(user);
    wHistory.setVisibleSize(42);
    wTitle.setContent(string("#")
                                .append(channel.getName())
                                .append(" - ")
                                .append(channel.getTopic())
                            );

    do {
        switch(ch) {   
            case 0: // first run!
                // do nothing here for now.
                break;

            case 263: // backspace
                input.deleteLastChar();
                break;

            case 10: // send message to server!
                if (input.str().size() == 0) {
                    // empty message; don't do anything
                    break;
                }
                
                if (input.str().at(0) == '/') {
                    // this is a command!
                    string command = input.str().substr(1, input.str().find(' ')-1);
                    if (command == "nick") {
                        // changing nick.
                        string newNick = input.str().substr(input.str().find(' ')+1);
                        user->setNick(newNick);

                        wNames.redraw();
                    } else if (command == "topic") {
                        // changing topic 
                        string topic = input.str().substr(input.str().find(' ')+1);
                        channel.setTopic(topic);

                        wTitle.setContent(string("#")
                                                .append(channel.getName())
                                                .append(" - ")
                                                .append(channel.getTopic())
                                                );
                    }
                } else {
                    // this is a message.
                    // put it in the channel history list
                    Message* message = new Message(user, input.str());
                    wHistory.addItem(message);
                }

                input.clearInput(); 
                break;
            
            default:
                input.putChar(ch);
                break;
        } 
    } while((ch = input.getChar()) != KEY_END);

    endwin();           /* End curses mode        */
    return 0;
}
