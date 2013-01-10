#ifndef IRCSOCKET_H
#define IRCSOCKET_H

#include "../include/typedef.h"

class IRCSocket {
    public:
        IRCSocket();

        ~IRCSocket();

        void server(std::string host, unsigned short pot);

        void connect();

        void send(std::string message);

        void read(std::string& message);
        
        void start(UI* ui, User* user);

        void close();

    private:
        ConnectionHandler* _ch;
};

#endif
