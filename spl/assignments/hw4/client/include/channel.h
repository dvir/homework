#ifndef CHANNEL_H
#define CHANNEL_H

#include "../include/typedef.h"

class Channel {
    public:
        Channel(std::string name);

        virtual ~Channel();

        void setTopic(std::string topic);

        void addUser(User_ptr user);

        /**
         * Add a list of users.
        **/
        virtual void addUsers(Users users);

        void removeUser(User_ptr user);

        size_t getUsersCount() const;

        std::string getName() const;

        std::string getTopic() const;

        Users getUsers() const;

        std::string toString() const;

    private:
        std::string _name;
        std::string _topic;
        Users _users;
};

#endif
