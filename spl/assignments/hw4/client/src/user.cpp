#include "../include/user.h"

#include <string>
#include <vector>
#include <algorithm>

User::User(std::string nick) :
    _nick(nick),
    _name(nick),
    _chanMode(""),
    _channels()
{
    // detect channel modes in the nick and set them 
    // accordingly.
    if (nick.at(0) == '@' || nick.at(0) == '+') {
        this->_chanMode = this->_nick.at(0);
        this->_nick = this->_nick.substr(1);
    }
}

User::User(User& other) :
    _nick(other.getNick()),
    _name(other.getName()),
    _chanMode(other.getChanMode()),
    _channels(other.getChannels())
{           
}

void User::addChannel(Channel* channel) {
    _channels.push_back(channel);
}

void User::removeChannel(Channel* channel) {
    Channels::iterator position = std::find(
                                        _channels.begin(), 
                                        _channels.end(),
                                        channel
                                        );
    if (position != _channels.end()) {
        _channels.erase(position);
    }
}

bool User::isInChannel(Channel* channel) {
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
    return this->getFullNick() < rhs.getFullNick();
}


bool UserPointerCompare (const User* l, const User* r) {
    return *l < *r;
};
