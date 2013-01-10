#include "../include/ui.h"
#include "../include/utils.h"
#include "../include/contentwindow.h"
#include "../include/inputwindow.h"
#include "../include/listwindow.h"
#include "../include/user.h"
#include "../include/channel.h"

#include <algorithm>

UI::UI(ContentWindow<Channel*>* wTitle, 
   ListWindow<Message*>* wHistory, 
   ListWindow<User*>* wNames, 
   InputWindow* wInput) :
    title(wTitle),
    history(wHistory),
    names(wNames),
    input(wInput),
    _hasStartedNamesStream(false)
{
}

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
            User* newUser = new User(*it);
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

Channel* UI::getChannel() {
    return _channel;
}

void UI::setChannel(Channel* newChannel) {
    _channel = newChannel;
    this->title->setContent(_channel);
    this->names->removeAll();
}

void UI::addUser(User* newUser) {
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
