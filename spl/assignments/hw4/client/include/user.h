#ifndef USER_H
#define USER_H

#include "../include/typedef.h"
#include "../include/channel.h"

#include <boost/shared_ptr.hpp>

class User {
    public:
        static User_ptr getUser(std::string nick);
        static User_ptr getUser(std::string nick, bool create);

        User(User& other);

        void addChannel(Channel_ptr channel);

        void removeChannel(Channel_ptr channel);
        
        bool isInChannel(Channel_ptr channel);

        void setNick(std::string nick);

        void setName(std::string name);

        std::string getName() const;

        std::string getNick() const;

        Channels getChannels() const;

        std::string toString() const;

        std::string getChanMode() const;

        /**
         * Return nick containing channel mode.
        **/
        std::string getFullNick() const;
    
        bool operator <(const User& rhs) const;

    private:
        User(std::string nick);
        static Users _users;
        
        std::string _nick;
        std::string _name;
        std::string _chanMode;
        Channels _channels;
};

bool UserPointerCompare (const User_ptr l, const User_ptr r);

#endif
