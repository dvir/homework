#ifndef MESSAGE_H
#define MESSAGE_H

#include "../include/typedef.h"

class Message {
    public:
        enum Type { DEFAULT, PRIVATE, ACTION, SYSTEM, DEBUG };

        Message(std::string text);

        Message(std::string text, User* user);
        
        Message(std::string text, Message::Type type);

        Message(std::string text, User* user, Message::Type type);
        Message (Message& other);
        
        Message operator=(const Message& other);

        User* getUser() const;

        std::string getText() const;

        std::string getNick() const; 

        short getType() const;

        time_t getTimestamp() const;

        std::string getTimestampString() const;

        std::string toString() const;

    private:
        User* _user; // message associated user
        std::string _text; // message text
        std::string _nick; // associated nick
        time_t _timestamp; // timestamp of message arrival
        short _type; // type of message 
};

#endif
