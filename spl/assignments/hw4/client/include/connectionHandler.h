#ifndef CONNECTION_HANDLER_H
#define CONNECTION_HANDLER_H

#include "../include/typedef.h"

#include <boost/asio.hpp>

class ConnectionHandler {
    public:
        ConnectionHandler(std::string host, short port);
        ~ConnectionHandler();

        // Connect to the remote machine
        void connect();

        // Whether we are still connected to the host or not
        bool isConnected();

        // Read a fixed number of bytes from the server - blocking.
        // Returns false in case the connection is closed before bytesToRead bytes can be read.
        void getBytes(char bytes[], unsigned int bytesToRead);

        // Send a fixed number of bytes from the client - blocking.
        // Returns false in case the connection is closed before all the data is sent.
        void sendBytes(const char bytes[], int bytesToWrite);

        // Read an ascii line from the server
        // Returns false in case connection closed before a newline can be read.
        void read(std::string& line);

        // Send an ascii line from the server
        // Returns false in case connection closed before all the data is sent.
        void send(const std::string& line);

        // Get Ascii data from the server until the delimiter character
        // Returns false in case connection closed before null can be read.
        void getFrameAscii(std::string& frame, char delimiter);

        // Send a message to the remote host.
        // Returns false in case connection is closed before all the data is sent.
        void sendFrameAscii(const std::string& frame, char delimiter);

        // Close down the connection properly.
        void close();

    private:
        const std::string _host;
        const short _port;
        boost::asio::io_service _io_service;   // Provides core I/O functionality
        boost::asio::ip::tcp::socket _socket; 
        bool _connected;
};

#endif
