#define DBG

#include <curses.h>
#include <iostream>
#include <sstream>
#include <istream>
#include <ostream>
#include <string>
#include <vector>
#include <algorithm>
#include <iomanip>

#include <boost/thread.hpp>
#include <boost/shared_ptr.hpp>

#include "../include/typedef.h"

#include "../include/connectionHandler.h"
#include "../include/user.h"
#include "../include/channel.h"
#include "../include/message.h"
#include "../include/ircsocket.h"
#include "../include/ui.h"
#include "../include/window.h"
#include "../include/contentwindow.h"
#include "../include/listwindow.h"
#include "../include/inputwindow.h"
#include "../include/utils.h"

/**
 * Command line parameters:
 * ./bin/client host port [with_gui]
 */
int main(int argc, char *argv[])
{
    if (argc < 2 || argc > 3) {
        std::cout << "Usage: " << argv[0] << " host port [with_gui]" << std::endl;
        return 1;
    }

    std::string host(argv[1]);
    unsigned short port = atoi(argv[2]);
    bool GUI = true;
    if (argc == 3) {
        GUI = false;
    }

    /**
     * Thread group holding the server connection thread(s).
     * Makes it easier to .join_all() threads in the end of the program.
     */
    boost::thread_group thread_group;
   
    /**
     * Create current user object.
     */
    User_ptr user = User::getUser("");
  
    /**
     * If allowed, start GUI mode 
     */
    if (GUI) {
        initscr();          /* Start curses mode        */
        start_color();          /* Start the color functionality */
        cbreak();           /* Line buffering disabled, Pass on
                             * everty thing to me       */
        keypad(stdscr, TRUE);       /* I need that nifty F1     */
        noecho();

        assume_default_colors(COLOR_GREEN, COLOR_BLACK);
    }

    // input window (down bottom)
    InputWindow* wInput = new InputWindow("input", 3, 153, 45, 0);

    // names window (all across the right side of the UI)
    ListWindow<User_ptr>* wNames = new ListWindow<User_ptr>("names", 46, 17, 2, 152);
    
    // set options for names window
    wNames->addRefreshAfterWindow(wInput);
    wNames->setVisibleSize(44);
    wNames->setReverseList(false);
    wNames->addItem(user);

    // title window (all across the top of the UI)
    ContentWindow<Channel_ptr>* wTitle = new ContentWindow<Channel_ptr>("title", 3, 169, 0, 0);
   
    // set options for title window
    wTitle->addRefreshAfterWindow(wInput);

    // history window (huge window in the middle), holding channel messages
    // and server communication messages.
    ListWindow<Message_ptr>* wHistory = new ListWindow<Message_ptr>("main", 44, 153, 2, 0);
    
    // set options for history window
    wHistory->setPrintToScreen(true);
    wHistory->addRefreshAfterWindow(wInput);
    wHistory->setVisibleSize(42);
   
    // UI is a set of title, history, names and input windows.
    UI_ptr ui(new UI(wTitle, wHistory, wNames, wInput));
    
    // set UI and current user for the IRC client
    IRCSocket server(ui, user);
   
    // try to connect to the given host and port.
    server.server(host, port);
    
    ui->history->addItem(
        Message::createMessage(
            "Connecting to server...",
            Message::SYSTEM
        )
    );

    try {
        server.connect();
        
        // start server socket thread to handle data from the server.
        // allows for non-blocking stdin.
        boost::thread* serverSocketThread = new boost::thread(
                &IRCSocket::start, 
                &server 
        );

        thread_group.add_thread(serverSocketThread);

    } catch (std::exception& e) {
        ui->history->addItem(
            Message::createMessage(
                "Connection failed!",
                Message::SYSTEM
            )
        );
    }
    
    bool exit = false;

    if (GUI) {
        /** with curses: **/
        std::string line; 
        short ch = 0;
        do {
            switch(ch) {   
                case 0: // first run!
                    // do nothing here for now.
                    break;

                case 263: // backspace
                    ui->input->deleteLastChar();
                    break;

                case 10: { // send message to server!
                    
                    line = ui->input->str();
                    ui->input->clearInput(); 
                    if (line.size() == 0) {
                        // empty message; don't do anything
                        break;
                    }
                    
                    std::string response;

                    try {
                            
                        IRCSocket::ClientCommand command = IRCSocket::parseClientCommand(line);

                        // if it's any of client only commands, handle here
                        // and don't send to the protocol.
                        if (command.name == "clear") {
                            ui->history->removeAll();
                        } else if (command.name == "raw") { // allow sending raw messages to server
                            response = command.params;
                        } else if (command.name == "debug") { // toggle debug messages
                            //_debug = !_debug;
                        } else if (command.name == "exit") {
                            // exit client cleanly!
                            ui->history->addItem(
                                Message::createMessage(
                                    "Shutting down...",
                                    Message::SYSTEM
                                )
                            );
                           
                            // first, disconnect from server if we are logged in.
                            if (server.isConnected()) {
                                response = server.clientCommand("/quit");
                            }

                            // stop stdin loop
                            exit = true;
                        } else if (command.name == "server") {
                            // change servers.
                            // first, disconnect from current server
                            if (server.isConnected()) {
                                response = server.clientCommand("/quit");
                            }

                            // construct host information from params
                            Strings params = Utils::split(command.params, ' ');
                            std::string host = params[0];
                            unsigned short port = atoi(params[1].c_str());

                            // try to connect to the new server.
                            server.server(host, port);
                            
                            ui->history->addItem(
                                Message::createMessage(
                                    "Connecting to server...",
                                    Message::SYSTEM
                                )
                            );

                            try {
                                server.connect();
                                
                                // start server socket thread to handle data from the server.
                                // allows for non-blocking stdin.
                                boost::thread* serverSocketThread = new boost::thread(
                                        &IRCSocket::start, 
                                        &server 
                                );

                                thread_group.add_thread(serverSocketThread);

                            } catch (std::exception& e) {
                                ui->history->addItem(
                                    Message::createMessage(
                                        "Connection failed!",
                                        Message::SYSTEM
                                    )
                                );
                            }
                        } else if (command.name == "chanmsg") { 
                            // if we got here, this is a message.
                            // check if we are already in a channel
                            if (!ui->getChannel()) {
                                // no active channel! 
                                // alert the user that he should join first
                                throw (
                                    std::logic_error(
                                        "Can't send message - no active channels"
                                    )
                                );
                            }
                    
                            // put it in the channel history list
                            ui->history->addItem(
                                    Message::createMessage(
                                        line,
                                        user
                                    )
                            );

                            // transmit message to the server
                            response = server.message(
                                std::string(ui->getChannel()->getName())
                                .append(" ")
                                .append(line)
                            );
                        } else {
                            // not any of client only commands.
                            // call the protocol.
                            //
                            // but, check first if we are connected
                            // to a server.
                            if (!server.isConnected()) {
                                throw (
                                    std::logic_error(
                                        "Connect to a server first"
                                    )
                                );
                            }

                            response = server.clientCommand(line);
                        }

                    } catch (std::exception& error) {
                        // display error to client
                        ui->history->addItem(
                            Message::createMessage(
                                error.what(),
                                Message::ERROR
                            ) 
                        );

                    }

                    if (response != "") {
                        server.send(response);
                    }

                    break; // end enter case
                }

                default:
                    if (ch >= 32 && ch <= 126) { // valid ascii character
                        ui->input->putChar(ch);
                    }
                    break;
            } 
        } while (!exit && (ch = ui->input->getChar()) != KEY_END);
    } else {
        /** without GUI **/
        char buf[512];
        while (!exit && std::cin.getline(buf, 512)) {
            std::string line(buf);
            
            if (line.size() == 0) {
                // empty message; don't do anything
                break;
            }
            
            std::string response;

            try {
                   
                // parse the client command. if it isn't starting
                // with a slash ('/') it will be regarded
                // as a channel message.
                IRCSocket::ClientCommand command = IRCSocket::parseClientCommand(line);
                
                // if it's any of client only commands, handle here
                // and don't send to the protocol.
                if (command.name == "clear") {
                    ui->history->removeAll();
                } else if (command.name == "raw") { // allow sending raw messages to server
                    response = command.params;
                } else if (command.name == "debug") { // toggle debug messages
                    //_debug = !_debug;
                } else if (command.name == "exit") {
                    // exit client cleanly!
                    ui->history->addItem(
                        Message::createMessage(
                            "Shutting down...",
                            Message::SYSTEM
                        )
                    );
                   
                    // first, disconnect from server if we are logged in.
                    if (server.isConnected()) {
                        response = server.clientCommand("/quit");
                    }

                    // stop stdin loop
                    exit = true;
                } else if (command.name == "server") {
                    // change servers.
                    // first, disconnect from current server
                    if (server.isConnected()) {
                        response = server.clientCommand("/quit");
                    }

                    // construct host information from params
                    Strings params = Utils::split(command.params, ' ');
                    std::string host = params[0];
                    unsigned short port = atoi(params[1].c_str());

                    // try to connect to the new server.
                    // NOTE: the while construct is used for its
                    // break feature;
                    server.server(host, port);
                    
                    ui->history->addItem(
                        Message::createMessage(
                            "Connecting to server...",
                            Message::SYSTEM
                        )
                    );
                    try {
                        server.connect();

                        // start server socket thread to handle data from the server.
                        // allows for non-blocking stdin.
                        boost::thread* serverSocketThread = new boost::thread(
                                &IRCSocket::start, 
                                &server 
                        );

                        thread_group.add_thread(serverSocketThread);

                    } catch (std::exception& e) {
                        ui->history->addItem(
                            Message::createMessage(
                                "Connection failed!",
                                Message::SYSTEM
                            )
                        );
                    }
                } else if (command.name == "chanmsg") { 
                    // if we got here, this is a message.
                    // check if we are already in a channel
                    if (!ui->getChannel()) {
                        // no active channel! 
                        // alert the user that he should join first
                        throw (
                            std::logic_error(
                                "Can't send message - no active channels"
                            )
                        );
                    }
            
                    // put it in the channel history list
                    ui->history->addItem(
                            Message::createMessage(
                                line,
                                user
                            )
                    );


                    // transmit message to the server
                    response = server.message(
                        std::string(ui->getChannel()->getName())
                        .append(" ")
                        .append(line)
                    );
                } else {
                    // not any of client only commands.
                    // call the protocol.
                    //
                    // but, check first if we are connected
                    // to a server.
                    if (!server.isConnected()) {
                        throw (
                            std::logic_error(
                                "Connect to a server first"
                            )
                        );
                    }

                    response = server.clientCommand(line);
                }

            } catch (std::exception& error) {
                // display error to client
                ui->history->addItem(
                    Message::createMessage(
                        error.what(),
                        Message::ERROR
                    ) 
                );

            }

            if (response != "") {
                server.send(response);
            }
        }
    }

    // wait for all threads to finish
    thread_group.join_all();

    // end curses mode
    endwin();

    // clean
    delete wInput;
    delete wHistory;
    delete wNames;
    delete wTitle;

    return 0;
}
