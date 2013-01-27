#include "../include/ui.h"
#include "../include/utils.h"
#include "../include/contentwindow.h"
#include "../include/inputwindow.h"
#include "../include/listwindow.h"
#include "../include/user.h"
#include "../include/channel.h"

#include <algorithm>

UI::UI(ContentWindow<Channel_ptr>* wTitle, 
   ListWindow<Message_ptr>* wHistory, 
   ListWindow<User_ptr>* wNames, 
   InputWindow* wInput) :
    title(wTitle),
    history(wHistory),
    names(wNames),
    input(wInput),
    _hasStartedNamesStream(false),
    _namesStream(),
    _channel(Channel_ptr())
{
}

UI::UI (const UI& other) :
    title(other.title),
    history(other.history),
    names(other.names),
    input(other.input),
    _hasStartedNamesStream(other._hasStartedNamesStream),
    _namesStream(other._namesStream),
    _channel(other._channel)
{
}

UI& UI::operator=(const UI& other) {
	if (this == &other) {
		return *this;
	}
	
        this->title = other.title;
        this->history = other.history;
        this->names = other.names;
        this->input = other.input;
        this->_hasStartedNamesStream = other._hasStartedNamesStream;
        this->_namesStream = other._namesStream;
        this->_channel = other._channel;
	return *this;
}

UI::~UI() {
};

void UI::reset() {
    this->setChannel(Channel_ptr());
}

void UI::startNamesStream() {
    if (false == _hasStartedNamesStream) {
        _hasStartedNamesStream = true;
        std::cout << "NAMES:" << std::endl << std::flush;
    }
}

void UI::endNamesStream() {
    _hasStartedNamesStream = false;

    if (_namesStream.size() > 0) {
        this->names->removeAll();

        Users users;
        for (Strings::iterator it = _namesStream.begin();
             it != _namesStream.end();
             ++it)
        {
            std::string nick = *it;
            User_ptr newUser = User::getUser(nick);
            std::string chanMode;
            if (nick.at(0) == '@' || nick.at(0) == '+') {
                chanMode = nick.substr(0, 1);
            }
            newUser->setChanMode(chanMode);
            users.push_back(newUser);
        }

        sort(users.begin(), users.end(), User::PointerCompare);
        this->addUsers(users);
        _namesStream.clear();
    }
}

void UI::addNames(std::string str) {
    Strings names = Utils::split(Utils::trim(str), ' ');
    for (Strings::iterator it = names.begin();
         it != names.end();
         ++it)
    {
        _namesStream.push_back(*it);
    }
}

Channel_ptr UI::getChannel() {
    return _channel;
}

void UI::setChannel(Channel_ptr newChannel) {
    _channel = newChannel;
    this->title->setContent(_channel);
    this->names->removeAll();
}

void UI::addUser(User_ptr newUser) {
    this->names->addItem(newUser);

    if (this->getChannel()) {
        this->getChannel()->addUser(newUser);
    }
}

void UI::addUsers(Users users) {
    this->names->addItems(users);
    
    if (this->getChannel()) {
        this->getChannel()->addUsers(users);
    }
}
