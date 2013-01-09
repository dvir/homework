#ifndef CHANNEL_H
#define CHANNEL_H

#include "../include/typedef.h"

#include <string>

class Channel {
    public:
        Channel(std::string name) : 
            _name(name), 
            _topic(""), 
            _users() 
        {
        };

        virtual ~Channel() {
        };

        void setTopic(std::string topic) {
            this->_topic = topic;  
        };

        void addUser(User* user) {
            this->_users.push_back(user);
        };

        /**
         * Add a list of users.
        **/
        virtual void addUsers(Users users) {
            for (Users::iterator it = users.begin();
                 it != users.end();
                 ++it)
            {
                this->addUser(*it);
            }
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
            return std::string()
                    .append(this->getName())
                    .append(" - ")
                    .append(this->getTopic())
                  ;
        };

    private:
        std::string _name;
        std::string _topic;
        Users _users;
};

#endif
