#ifndef MESSAGE_H
#define MESSAGE_H

#include "../include/typedef.h"

class Message {
    public:
        enum Type { 
            DEFAULT, 
            PRIVATE, 
            ACTION, 
            SYSTEM, 
            DEBUG, 
            ERROR 
        };

        /**
         * Static methods that create message object.
         * NOTE: Factory-like. Constructors are private.
         */
        static Message_ptr createMessage(std::string text);
        static Message_ptr createMessage(std::string text, User_ptr user);
        static Message_ptr createMessage(std::string text, Message::Type type);
        static Message_ptr createMessage(std::string text, User_ptr user, Message::Type type);

        Message operator=(const Message& other);

        /**
         * Returns the user object associated with the message.
         * NOTE: not all messages are associated to a user.
         */
        User_ptr getUser() const;
        
        /**
         * Returns the message text.
         */
        std::string getText() const;

        /**
         * Returns the nick associated with the message.
         * The nick string is being set when the message is created, 
         * so changes to the user/object associated with it won't
         * affect the message.
         */
        std::string getNick() const; 

        /**
         * Get the message type.
         */
        Type getType() const;

        /**
         * Get the message creation timestamp.
         */
        time_t getTimestamp() const;

        /**
         * Get a string representing the message creation timestamp,
         * in the format: HH:mm
         */
        std::string getTimestampString() const;

        /**
         * String representation of the message object.
         * Chanages according to the message type, and shows the 
         * associated nick and text.
         */
        std::string toString() const;

    private:
        /**
         * Construct a message.
         * Not all message objects are associated with a user, 
         * and some may use the default type, therefore there are a few
         * possible constructors.
         *
         * NOTE: they are all private because Message is a factory class.
         */
        Message(std::string text);
        Message(std::string text, User_ptr user);
        Message(std::string text, Message::Type type);
        Message(std::string text, User_ptr user, Message::Type type);
        Message (Message& other);
        
        User_ptr _user; // message associated user
        std::string _text; // message text
        std::string _nick; // associated nick
        time_t _timestamp; // timestamp of message arrival
        Type _type; // type of message 
};

#endif
