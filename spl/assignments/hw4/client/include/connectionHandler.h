#ifndef CONNECTION_HANDLER_H
#define CONNECTION_HANDLER_H

#include "../include/typedef.h"

#include <boost/asio.hpp>

class ConnectionHandler {
    public:
        /**
         * Construct a ConnectionHandler with a given host and port.
         */
        ConnectionHandler(std::string host, short port);

        /**
         * Destroy a ConnectionHandler by closing the socket connection.
         */
        ~ConnectionHandler();

        /**
         * Initiates connection process to the given host and port.
         * Tries to resolve host names.
         */
        void connect();

        /**
         * Whether we are still connected to the host or not
         */
        bool isConnected();

        /**
         * Read a fixed number of bytes from the server - blocking.
         * @throws an exception if an error occurred.
         */
        void getBytes(char bytes[], unsigned int bytesToRead);

        /**
         * Send a fixed number of bytes from the client - blocking.
         * @throws an exception if an error occurred.
         */
        void sendBytes(const char bytes[], int bytesToWrite);

        /**
         * Read an ascii line from the server
         * @throws an exception if an error occurred.
         */
        void read(std::string& line);

        /**
         * Send an ascii line from the server
         * @throws an exception if an error occurred.
         */
        void send(const std::string& line);

        /**
         * Get Ascii data from the server until the delimiter character
         * @throws an exception if an error occurred.
         */
        void getFrameAscii(std::string& frame, char delimiter);

        /**
         * Send a message to the remote host.
         * @throws an exception if an error occurred.
         */
        void sendFrameAscii(const std::string& frame, char delimiter);

        // Close down the connection properly.
        void close();

    private:
        /** Current host name/address. **/
        const std::string _host;

        /** Current host port. **/
        const short _port;

        /** IO service. **/
        boost::asio::io_service _io_service;

        /** Connection socket. **/
        boost::asio::ip::tcp::socket _socket; 

        /** Connection state. **/
        bool _connected;
};

#endif
