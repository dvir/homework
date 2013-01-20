#ifndef USER_H
#define USER_H

#include "../include/typedef.h"
#include "../include/channel.h"

#include <boost/shared_ptr.hpp>

class User {
    public:
        /**
         * Function used to compare between two user pointers.
         */
        static bool PointerCompare (const User_ptr l, const User_ptr r);

        /** 
         * Static methods that create a user object
         * or returns an instance depending on the given nick.
         * NOTE: Factory-like. Constructor is private.
         */
        static User_ptr getUser(std::string nick);
        static User_ptr getUser(std::string nick, bool create);
        static User_ptr getUser(const User& user);

        /**
         * Used to compare (and sort) users in a list.
         */
        bool operator <(const User& rhs) const;
        
        /**
         * Add a channel to the user's current channels list.
         */
        void addChannel(Channel_ptr channel);

        /**
         * Remove a channel from the user's current channels list.
         */
        void removeChannel(Channel_ptr channel);
        
        /**
         * Returns whether the user is currently in a given channel.
         */
        bool isInChannel(Channel_ptr channel);

        /**
         * The user initiated a NICK command and is awaiting
         * confirmation. (401 NICK_ACCEPTED)
         */
        void setPendingNick(std::string nick);

        /**
         * Pending nick was accepted.
         */
        void nickAccepted();

        /**
         * Set the user's nick.
         */
        void setNick(std::string nick);

        /**
         * Set the user's name.
         */
        void setName(std::string name);

        /**
         * Set the user's channel mode.
         * May be either '@' or '+'.
         */
        void setChanMode(std::string mode);

        
        /**
         * Return the user's nick containing channel mode.
        **/
        std::string getFullNick() const;

        /**
         * Returns the user's name.
         */
        std::string getName() const;

        /**
         * Returns the user's nick. (without channel mode)
         */
        std::string getNick() const;

        /**
         * Returns the user's channel list.
         */
        Channels getChannels() const;

        /** 
         * Returns the user chan mode.
         */
        std::string getChanMode() const;
       
        /**
         * Returns a string representation of the user.
         * [chanMode]nick
         */
        std::string toString() const;

    private:
        /**
         * Construct a user from a nick string.
         * If a chan mode is preceding the nick, set it and remove from
         * the actual nick.
         *
         * NOTE: private because User is a factory class.
         */
        User(std::string nick);
        User(const User& other);

        /**
         * List of users created by our factory model.
         */
        static Users _users;
       
        std::string _nick;
        std::string _name;
        std::string _chanMode;
        Channels _channels;
        std::string _pendingNick;
};

#endif
