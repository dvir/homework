#include <curses.h>
#include <iostream>
#include <sstream>
#include <istream>
#include <ostream>
#include <string>
#include <vector>
#include <algorithm>
#include <iomanip>

#include <boost/thread.hpp>

#include "../include/connectionHandler.h"

using namespace std;

class User;
class Channel;
class Message;

typedef vector<std::string> Names;
typedef vector<std::string> History;
typedef vector<User*> Users;
typedef vector<Channel*> Channels;

/**
 * Wordwrap a @param str (insert \n) every @param width characters.
 * @param std::string str String to wordwrap.
 * @param size_t width    Width to wordwrap it to.
 * @return std::string Wordwrapped string.
**/
std::string wordwrap(const std::string& str, size_t width) {
    std::string newString(str);
    for (size_t ii = width; ii < newString.size(); ii += width) {
        newString.insert(ii, "\n");
    }

    return newString;
}

std::vector<std::string> &split(const std::string &s, char delim, std::vector<std::string> &elems) {
    std::stringstream ss(s);
    std::string item;
    while(std::getline(ss, item, delim)) {
        elems.push_back(item);
    }
    return elems;
}

std::vector<std::string> split(const std::string &s, char delim) {
    std::vector<std::string> elems;
    return split(s, delim, elems);
}

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

        std::string toString() const {
            return string("#")
                    .append(getName())
                    .append(" - ")
                    .append(getTopic())
                  ;
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
            _nick(user->getNick()),
            _timestamp(time(0))
        {
        };

        Message (Message& other) :
            _user(other.getUser()),
            _text(other.getText()),
            _nick(other.getNick()),
            _timestamp(other.getTimestamp())
        {
        };
        
        Message operator=(const Message& other) {
            _user = new User("Copy", "John Copy");
            _text = "copy";
            _nick = "Copy-nick";

            return *this;
        };

        User* getUser() {
            return _user;
        };

        std::string getText() const {
            return _text;
        };

        std::string getNick() const {
            return _nick;
        };

        time_t getTimestamp() const {
            return _timestamp;
        };

        std::string getTimestampString() const {
            struct tm* now = localtime(&_timestamp);
            ostringstream time;
            time << std::setfill('0') << std::setw(2) << (1 + now->tm_hour) 
                 << ":" 
                 << std::setfill('0') << std::setw(2) << now->tm_min;
            return time.str();
        };

        std::string toString() {
            return string()
                    .append(getTimestampString())
                    .append(" <")
                    .append(this->getNick())
                    .append("> ")
                    .append(this->getText());
        };

    private:
        User* _user;
        std::string _text;
        std::string _nick;
        time_t _timestamp;
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
            _startx(startx),
            _returnCursorWindow(NULL)
        {
            // allow detections of special keys
            keypad(_win, TRUE);

            this->setup();

            this->refreshWindow();
        };

        virtual void setup() {
            // draw window borders
            wborder(_win, '|', '|', '-', '-', '+', '+', '+', '+');
        };

        virtual void refreshWindow() {
            wrefresh(_win);
            
            if (NULL != _returnCursorWindow) {
               _returnCursorWindow->refreshCursor(); 
            }
        };

        virtual void print(std::string text) {
            wprintw(_win, text.c_str());

            if (NULL != _returnCursorWindow) {
               _returnCursorWindow->refreshCursor(); 
            }
        };

        virtual void print(int y, int x, std::string text) {
            mvwprintw(_win, y, x, text.c_str());

            if (NULL != _returnCursorWindow) {
               _returnCursorWindow->refreshCursor(); 
            }
        };

        virtual void refreshCursor() {
            // it is enough to refresh the window in order
            // for the cursor to re-appear in its correct position in it.
            this->refreshWindow();
        };

        virtual void setReturnCursorWindow(Window* returnCursorWindow) {
            _returnCursorWindow = returnCursorWindow;
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
        Window* _returnCursorWindow;
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

template <typename T>
class ListWindow : public ContentWindow<T> {
    public:
        ListWindow(
            std::string name, 
            int height, 
            int width, 
            int starty, 
            int startx
        ) :
            ContentWindow<T>(name, height, width, starty, startx),
            _list(),
            _visibleSize(-1)
        {
        }

        virtual void redraw() {
            this->clear();
            
            // count, from the bottom up, how many lines each item string
            // representation will require. get up to _visibleSize lines
            // and then spit them out at once.
            
            std::vector<std::string> reversedOutputLines;
            size_t linesCount = 0;
            for (typename std::vector<T>::reverse_iterator r_it = _list.rbegin();
                 r_it != _list.rend() && linesCount < _visibleSize;
                 ++r_it)
            {
                // wordwrap the text at _wrapWidth 
                // and then split it to lines.
                std::vector<std::string> lines = split(wordwrap((*r_it)->toString(), 150), '\n');

                // reverse iterate on the lines
                // and add them to the output lines.
                for (int jj = lines.size()-1; 
                     jj >= 0 && linesCount < _visibleSize;
                     --jj) 
                {
                    reversedOutputLines.push_back(lines[jj]); 
                    linesCount++;
                }
            }

            int ii = 0;
            for (std::vector<std::string>::reverse_iterator r_it = reversedOutputLines.rbegin();
                 r_it != reversedOutputLines.rend();
                 ++r_it, ++ii)
            {
                this->print(this->getOffsetY() + ii, this->getOffsetX(), (*r_it));
            }
                 
            this->refreshWindow();
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

        virtual size_t size() {
            return _list.size();
        };
        
    protected:
        std::vector<T> _list;
        size_t _visibleSize;
};

template <typename T>
bool from_string(T& t,
                const std::string& s,
                std::ios_base& (*f)(std::ios_base&) = std::dec)
{
    std::istringstream iss(s);
    return !(iss >> f >> t).fail();
}

class Network {
    public:
        Network(ConnectionHandler* ch) : 
            _ch(ch)
        {
        }

        void read(ListWindow<Message*>* wList) { 
            std::string answer;
            while (_ch->read(answer)) {
                wList->addItem(new Message(new User("server", "Server"), answer));    
                if (answer.substr(0, 4) == "PING") {
                    _ch->send(answer.replace(1, 1, "O"));
                }

                answer.clear();
            }
        };

    private:
        ConnectionHandler* _ch;
};

int main(int argc, char *argv[])
{
    
    if (argc != 4) {
        cout << "Usage: " << argv[0] << " host port nick" << std::endl;
        return 1;
    }

    string host(argv[1]);
    unsigned short port = atoi(argv[2]);
    string nick(argv[3]);

    std::cout << "Creating connection..." << std::endl;
    ConnectionHandler server(host, port);
    std::cout << "Connecting to server..." << std::endl;
    if (!server.connect()) {
        std::cout << "Cannot connect to " << host << ":" << port << std::endl;
        return 1;
    }
    
    User* user = new User(nick, nick);

    ostringstream line;
    line << "NICK " << user->getNick() << std::endl;
    if (!server.send(line.str())) {
        std::cout << "Disconnected. Exiting..." << std::endl;
        return 0;
    }
   
    line.str("");
    line.clear();
    line << "USER " << user->getNick() << " 0 * :" << user->getName() << std::endl;
    if (!server.send(line.str())) {
        std::cout << "Disconnected. Exiting..." << std::endl;
        return 0;
    }

    Channel* channel = new Channel("SERVER");
    channel->setTopic("/join a channel!");
    
    int ch = 0;

    initscr();          /* Start curses mode        */
    start_color();          /* Start the color functionality */
    cbreak();           /* Line buffering disabled, Pass on
                         * everty thing to me       */
    keypad(stdscr, TRUE);       /* I need that nifty F1     */
    noecho();
    init_pair(1, COLOR_GREEN, COLOR_BLACK);

    assume_default_colors(COLOR_GREEN, COLOR_BLACK);
    
//    attron(COLOR_PAIR(1));
//    mvprintw(0, 0, "Press End to exit");
//    refresh();
//    attroff(COLOR_PAIR(1));
    
    InputWindow* input = new InputWindow("input", 3, 153, 45, 0);
    
    ListWindow<User*>* wNames = new ListWindow<User*>("names", 46, 17, 2, 152);
    wNames->setReturnCursorWindow(input);
    wNames->addItem(user);
    
    ContentWindow<Channel*>* wTitle = new ContentWindow<Channel*>("title", 3, 169, 0, 0);
    wTitle->setReturnCursorWindow(input);
    wTitle->setContent(channel);

    ListWindow<Message*>* wHistory = new ListWindow<Message*>("main", 44, 153, 2, 0);
    wHistory->setReturnCursorWindow(input);
    wHistory->setVisibleSize(42);
    wHistory->addItem(new Message(new User("startup", "startup"), "*** Starting... ***"));
    
    Network networkRead(&server);
    boost::thread networkInputThread(&Network::read, &networkRead, wHistory);
    
    do {
        switch(ch) {   
            case 0: // first run!
                // do nothing here for now.
                break;

            case 263: // backspace
                input->deleteLastChar();
                break;

            case 10: // send message to server!
                if (input->str().size() == 0) {
                    // empty message; don't do anything
                    break;
                }
                
                if (input->str().at(0) == '/') {
                    // this is a command!
                    string command = input->str().substr(1, input->str().find(' ')-1);
                    string params = input->str().substr(input->str().find(' ')+1);
                    if (command == "nick") {
                        // changing nick.
                        server.send(string("NICK ").append(params));
                    } else if (command == "join") {
                        channel = new Channel(params);
                        wTitle->setContent(channel);
                    } else if (command == "topic") {
                        // changing topic 
                        channel->setTopic(params);
                        wTitle->redraw();
                    } else if (command == "quit") {
                        server.send(string("QUIT :").append(params));
                    }
                } else {
                    // this is a message.
                    // put it in the channel history list
                    Message* message = new Message(user, input->str());
                    wHistory->addItem(message);

                    // transmit message to the server
                    server.send(input->str()); 
                }

                input->clearInput(); 
                break;
            
            default:
                input->putChar(ch);
                break;
        } 
    } while((ch = input->getChar()) != KEY_END);

    networkInputThread.join();
    
    // end curses mode
    endwin();

    return 0;
}
