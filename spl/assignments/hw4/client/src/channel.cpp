#include "../include/channel.h"
#include "../include/user.h"

#include <string>
#include <algorithm>

Channel::Channel(std::string name) : 
    _name(name), 
    _topic(""), 
    _users() 
{
}

Channel::~Channel() {
}

void Channel::setTopic(std::string topic) {
    this->_topic = topic;  
}

void Channel::addUser(User_ptr user) {
    this->_users.push_back(user);
}

/**
 * Add a list of users.
**/
void Channel::addUsers(Users users) {
    for (Users::iterator it = users.begin();
         it != users.end();
         ++it)
    {
        this->addUser(*it);
    }
}

void Channel::removeUser(User_ptr user) {
    Users::iterator position = std::find(
                                    this->_users.begin(), 
                                    this->_users.end(),
                                    user
                                );

    if (position != _users.end()) {
        this->_users.erase(position);
    }
}

size_t Channel::getUsersCount() const {
    return this->_users.size();
}

std::string Channel::getName() const {
    return this->_name;
}

std::string Channel::getTopic() const {
    return this->_topic;
}

Users Channel::getUsers() const {
    return this->_users;
}

std::string Channel::toString() const {
    return std::string()
            .append(getName())
            .append(" - ")
            .append(getTopic())
          ;
}
