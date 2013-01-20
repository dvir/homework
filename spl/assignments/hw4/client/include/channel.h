#ifndef CHANNEL_H
#define CHANNEL_H

#include "../include/typedef.h"

class Channel {
    public:
        /**
         * Static methods that create a channel object
         * or returns an instance depending on the given channel
         * name. NOTE: Factory-like. Constructor is private.
         */
        static Channel_ptr getChannel(std::string name);
        static Channel_ptr getChannel(std::string name, bool create);
       
        /**
         * Set the channel topic to a given topic.
         */
        void setTopic(std::string topic);

        /**
         * Add a user to the channel users list.
         */
        void addUser(User_ptr user);
        
        /**
         * Remove a user from the channel users list.
         */
        void removeUser(User_ptr user);

        /**
         * Add a list of users.
         * Used to avoid repeating actions for each user.
         */
        void addUsers(Users users);

        /**
         * Returns amount of users currently in the channel.
         */
        size_t getUsersCount() const;
        
        /**
         * Returns the channel name.
         */
        std::string getName() const;

        /**
         * Returns the channel current topic.
         */
        std::string getTopic() const;

        /**
         * Returns a list of users currently in the channel.
         */
        Users getUsers() const;

        /**
         * Returns a string representation of the channel,
         * which is basically the channel name.
         */
        std::string toString() const;

    private:
        /**
         * Construct a channel from a given name string.
         *
         * NOTE: private because Channel is a factory class.
         */
        Channel(std::string name);
       
        /** 
         * List of channels created by our factory model.
         */
        static Channels _channels;
        
        std::string _name;
        std::string _topic;
        Users _users;
};

#endif
