#ifndef USER_H
#define USER_H

#include "../include/typedef.h"

#include <string>
#include <vector>

class User {
    public:
        User(std::string nick) :
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
        };

        User(User& other) :
            _nick(other.getNick()),
            _name(other.getName()),
            _chanMode(other.getChanMode()),
            _channels(other.getChannels())
        {           
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

        void setName(std::string name) {
            this->_name = name;
        };

        std::string getName() const {
            return this->_name;
        };

        std::string getNick() const {
            return this->_nick;
        };

        Channels getChannels() const {
            return this->_channels;
        };

        std::string toString() const {
            return this->getFullNick();
        };

        std::string getChanMode() const {
            return this->_chanMode;
        };

        /**
         * Return nick containing channel mode.
        **/
        std::string getFullNick() const {
            return std::string(this->getChanMode()).append(this->getNick());
        };
    
        bool operator <(const User& rhs) const {
            return this->getFullNick() < rhs.getFullNick();
        }

    private:
        std::string _nick;
        std::string _name;
        std::string _chanMode;
        Channels _channels;
};

bool UserPointerCompare (const User* l, const User* r) {
    return *l < *r;
};

#endif
