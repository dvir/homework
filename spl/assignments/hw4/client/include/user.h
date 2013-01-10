#ifndef USER_H
#define USER_H

#include "../include/typedef.h"

class User {
    public:
        User(std::string nick);

        User(User& other);

        void addChannel(Channel* channel);

        void removeChannel(Channel* channel);
        
        bool isInChannel(Channel* channel);

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
        std::string _nick;
        std::string _name;
        std::string _chanMode;
        Channels _channels;
};

bool UserPointerCompare (const User* l, const User* r);

#endif
