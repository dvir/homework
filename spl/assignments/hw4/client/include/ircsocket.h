#ifndef IRCSOCKET_H
#define IRCSOCKET_H

#include "../include/typedef.h"

class IRCSocket {
    public:
        struct ServerMessage {
            struct {
                std::string raw;
                std::string nick;
                std::string user;
                std::string host;
            } origin;

            std::string command;
            std::string target;
            std::string text;
        };

        struct ClientCommand {
            std::string name;
            std::string params;
        };

        IRCSocket(UI_ptr ui, User_ptr user);

        ~IRCSocket();

        void server(std::string host, unsigned short port);

        void connect();

        void send(std::string message);

        void read(std::string& message);
        
        void start();

        void close();
        
        /* CLIENT */

        static ClientCommand parseClientCommand(std::string line);

        std::string clientCommand(std::string line);
        
        void auth(std::string nick, std::string name);
        
        std::string nick(std::string params);
        
        std::string user(std::string params);
        std::string user(std::string nick, std::string name);

        std::string join(std::string target);

        std::string part(std::string params);
        std::string part(std::string target, std::string message);

        std::string topic(std::string params);
        std::string topic(std::string target, std::string message);

        std::string quit();

        std::string quit(std::string message);
        
        std::string message(std::string params);
        std::string message(std::string target, std::string params);

        /* SERVER */

        static ServerMessage parseServerMessage(std::string line);
    private:
        ConnectionHandler* _ch;
        UI_ptr _ui;
        User_ptr _user;
};

#endif
