#include "../include/typedef.h"
#include "../include/connectionHandler.h"

#include <string>
#include <iostream>

ConnectionHandler::ConnectionHandler(std::string host, short port) : 
    host_(host), 
    port_(port), 
    io_service_(), 
    socket_(io_service_)
{
}

ConnectionHandler::~ConnectionHandler() {
    this->close();
}

void ConnectionHandler::connect() {
    // create server endpoint
    boost::asio::ip::tcp::endpoint endpoint(
        boost::asio::ip::address::from_string(host_), 
        port_
    );
    
    boost::system::error_code error;
    socket_.connect(endpoint, error);

    if (error) {
        throw boost::system::system_error(error);
    }
}

void ConnectionHandler::getBytes(char bytes[], unsigned int bytesToRead) {
    size_t tmp = 0;
    boost::system::error_code error;
    while (!error && bytesToRead > tmp ) {
        tmp += socket_.read_some(boost::asio::buffer(bytes+tmp, bytesToRead-tmp), error);            
    }
    
    if (error) {
        throw boost::system::system_error(error);
    }
}

void ConnectionHandler::sendBytes(const char bytes[], int bytesToWrite) {
    int tmp = 0;
    boost::system::error_code error;
    
    while (!error && bytesToWrite > tmp ) {
        tmp += socket_.write_some(
                    boost::asio::buffer(
                        bytes + tmp, 
                        bytesToWrite - tmp
                    ), 
                    error
                );
    }
    
    if (error) {
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
    socket_.close();
}
