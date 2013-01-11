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
    _hasStartedNamesStream(false)
{
}

UI::~UI() {
};

void UI::startNamesStream() {
    if (false == _hasStartedNamesStream) {
        _hasStartedNamesStream = true;
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
            User_ptr newUser(new User(*it));
            users.push_back(newUser);
        }

        sort(users.begin(), users.end(), UserPointerCompare);
        this->addUsers(users);
        _namesStream.clear();
    }
}

void UI::addNames(std::string str) {
    Strings names = Utils::split(str, ' ');
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

    if (NULL != this->getChannel()) {
        this->getChannel()->addUser(newUser);
    }
}

void UI::addUsers(Users users) {
    this->names->addItems(users);
    
    if (NULL != this->getChannel()) {
        this->getChannel()->addUsers(users);
    }
}