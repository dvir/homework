#include "../include/typedef.h"
#include "../include/connectionHandler.h"

#include <string>
#include <iostream>
#include <cstdlib>

using boost::asio::ip::tcp;

ConnectionHandler::ConnectionHandler(std::string host, short port) : 
    _host(host), 
    _port(port), 
    _io_service(), 
    _socket(_io_service),
    _connected(false)
{
}

ConnectionHandler::~ConnectionHandler() {
    this->close();
}

void ConnectionHandler::connect() {
    // create server endpoint
    tcp::endpoint endpoint;    
   
    boost::system::error_code error;

    tcp::resolver resolver(_io_service);

    std::stringstream ss;
    ss << _port;
    tcp::resolver::query query(_host, ss.str());

    tcp::resolver::iterator end; // End marker.
    for (
        tcp::resolver::iterator it = resolver.resolve(query);
        it != end;
        ++it
        )
    {
        endpoint = *it;
        _socket.connect(endpoint, error);
        if (!error) {
            // connected successfully.
            // stop trying.
            _connected = true;
            return;
        }
    }

    if (error) {
        throw boost::system::system_error(error);
    }
}

bool ConnectionHandler::isConnected() {
    return _connected;
}

void ConnectionHandler::getBytes(char bytes[], unsigned int bytesToRead) {
    size_t tmp = 0;
    boost::system::error_code error;
    while (!error && bytesToRead > tmp ) {
        tmp += _socket.read_some(boost::asio::buffer(bytes+tmp, bytesToRead-tmp), error);            
    }
    
    if (error) {
        _connected = false;
        throw boost::system::system_error(error);
    }
}

void ConnectionHandler::sendBytes(const char bytes[], int bytesToWrite) {
    int tmp = 0;
    boost::system::error_code error;
    
    while (!error && bytesToWrite > tmp ) {
        tmp += _socket.write_some(
                    boost::asio::buffer(
                        bytes + tmp, 
                        bytesToWrite - tmp
                    ), 
                    error
                );
    }
    
    if (error) {
        _connected = false;
        throw boost::system::system_error(error);
    }
}

void ConnectionHandler::read(std::string& line) {
    getFrameAscii(line, '\n');
}

void ConnectionHandler::send(const std::string& line) {
    sendFrameAscii(line, '\n');
}

void ConnectionHandler::getFrameAscii(std::string& frame, char delimiter) {
    char ch;

    // Stop when we encounter the null character. 
    // Notice that the null character is not appended to the frame string.
    do {
        getBytes(&ch, 1);
        frame.append(1, ch);
    } while (delimiter != ch);
}

void ConnectionHandler::sendFrameAscii(const std::string& frame, char delimiter) {
    sendBytes(frame.c_str(), frame.length());
    sendBytes(&delimiter,1);
}

// Close down the connection properly.
void ConnectionHandler::close() {
    _socket.close();
    _connected = false;
}
