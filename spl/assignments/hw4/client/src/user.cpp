#include "../include/user.h"
#include "../include/channel.h"

#include <string>
#include <vector>
#include <algorithm>
#include <sstream>

Users User::_users = Users();

User_ptr User::getUser(std::string nick) {
    return User::getUser(nick, true);
}

User_ptr User::getUser(std::string nick, bool create) {
    std::string rawNick = nick;

    // detect channel modes in the nick 
    if (nick.at(0) == '@' || nick.at(0) == '+') {
        nick = nick.substr(1);
    }
    
    for (Users::iterator it = _users.begin(); it != _users.end(); ++it) {
        User_ptr user = *it;
        if (user->getNick() == nick) {
            return user;
        }
    }

    if (create) {
        // use rawNick in case the nick had any chan modes
        // attached to it.
        User_ptr newUser(new User(rawNick));
        _users.push_back(newUser);
        return newUser;
    }

    return User_ptr();
}

User::User(std::string nick) :
    _nick(nick),
    _name(nick),
    _chanMode(""),
    _channels()
{
    // detect channel modes in the nick and set them 
    // accordingly.
    if (nick.at(0) == '@' || nick.at(0) == '+') {
        this->_chanMode = nick.substr(0, 1);
        this->_nick = nick.substr(1);
        this->_name = this->_nick;
    }
}

User::User(User& other) :
    _nick(other.getNick()),
    _name(other.getName()),
    _chanMode(other.getChanMode()),
    _channels(other.getChannels())
{           
}

void User::addChannel(Channel_ptr channel) {
    _channels.push_back(channel);
}

void User::removeChannel(Channel_ptr channel) {
    Channels::iterator position = std::find(
                                        _channels.begin(), 
                                        _channels.end(),
                                        channel
                                        );
    if (position != _channels.end()) {
        _channels.erase(position);
    }
}

bool User::isInChannel(Channel_ptr channel) {
    Channels::iterator position = std::find(
                                        _channels.begin(), 
                                        _channels.end(),
                                        channel
                                        );
    return (position != _channels.end());
}

void User::setNick(std::string nick) {
    this->_nick = nick;
}

void User::setName(std::string name) {
    this->_name = name;
}

void User::setChanMode(std::string mode) {
    this->_chanMode = mode;
}

std::string User::getName() const {
    return this->_name;
}

std::string User::getNick() const {
    return this->_nick;
}

Channels User::getChannels() const {
    return this->_channels;
}

std::string User::toString() const {
    return this->getFullNick();
}

std::string User::getChanMode() const {
    return this->_chanMode;
}

/**
 * Return nick containing channel mode.
**/
std::string User::getFullNick() const {
    return std::string(this->getChanMode()).append(this->getNick());
}

bool User::operator<(const User& rhs) const {
    return (this->getChanMode() == "@" && rhs.getChanMode() != "@")
            || (this->getFullNick() < rhs.getFullNick());
}


bool UserPointerCompare (const User_ptr l, const User_ptr r) {
    return *l < *r;
};
