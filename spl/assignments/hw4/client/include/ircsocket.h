#ifndef IRCSOCKET_H
#define IRCSOCKET_H

#include "../include/typedef.h"

/**
 * Represents the IRC protocol socket handler.
 * In charge of replying to server requests and processing
 * client input, and handling the network communication
 * regarding IRC specifics.
 */
class IRCSocket {
    public:
        /**
         * A structure that holds different parts of a message received
         * from the server after it was processed.
         */
        struct ServerMessage {
            struct {
                std::string raw;
                std::string nick;
                std::string user;
                std::string host;
            } origin; // sender prefix details

            std::string raw; // the raw message
            std::string command; // the IRC command received
            std::string target; // target of the command
            std::string text; // additional text
        };

        struct ClientCommand {
            std::string name;
            std::string params;
        };

        /**
         * Constructor receiving a UI instance and a User object 
         * representing the current state of the application.
         */
        IRCSocket(UI_ptr ui, User_ptr user);

        /**
         * Destructor that cleans the socket before releasing.
         */
        ~IRCSocket();

        /**
         * Sets a new connection destination.
         */
        void server(std::string host, unsigned short port);

        /**
         * Connects to the previously chosen server:port.
         */
        void connect();

        /**
         * Returns true if we are currently connected to a server.
         */
        bool isConnected();

        /**
         * Sends a message to the server.
         */
        void send(std::string message);

        /**
         * Reads a message from the server.
         */
        void read(std::string& message);
        
        /**
         * Starts main read loop from the server.
         */
        void start();

        /**
         * Closes connection to the current server.
         */
        void close();
        
        /* CLIENT */

        /**
         * Parse user input.
         * Sentences starting with a slash ('/') are regarded
         * as command requests.
         */
        static ClientCommand parseClientCommand(std::string line);
    
        /**
         * Handle client command.
         */
        std::string clientCommand(std::string line);
        
        /**
         * Do the IRC authentication process. (NICK followed with a USER)
         */
        void auth(std::string nick, std::string name);
      
        /** CLIENT COMMANDS **/

        /**
         * Handle NICK command.
         * NICK new-nick
         */
        std::string nick(std::string params);
        
        /**
         * Handle USER command.
         * USER new-user
         */
        std::string user(std::string params);
        std::string user(std::string nick, std::string name);

        /**
         * Handle JOIN command.
         * JOIN #channel
         */
        std::string join(std::string target);

        /**
         * Handle PART command.
         * PART #channel
         */
        std::string part(std::string params);
        std::string part(std::string target, std::string message);

        /**
         * Handle TOPIC command.
         * TOPIC #channel new-topic
         */
        std::string topic(std::string params);
        std::string topic(std::string target, std::string message);

        /**
         * Handle QUIT command.
         * QUIT [message]
         */
        std::string quit();
        std::string quit(std::string message);
       
        /**
         * Handle channel messages.
         * PRIVMSG #channel :message
         */
        std::string message(std::string params);
        std::string message(std::string target, std::string params);

        /**
         * Handle NAMES command.
         * NAMES [#channel]
         */
        std::string names(std::string params);

        /**
         * Handle LIST command.
         * LIST
         */
        std::string list();

        /* SERVER */
        /**
         * Parse server messages into ServerMessage struct
         */
        static ServerMessage parseServerMessage(std::string line);

    private:
        /**
         * The ConnectionHandler associated with the current
         * user <-> server connection.
         */
        ConnectionHandler* _ch; 

        /**
         * The UI object representing the client GUI
         */
        UI_ptr _ui;

        /**
         * The current user associated with the client.
         */
        User_ptr _user;
};

#endif
