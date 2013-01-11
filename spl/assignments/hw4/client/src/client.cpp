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

int main(int argc, char *argv[])
{
    if (argc != 4) {
        std::cout << "Usage: " << argv[0] << " host port nick" << std::endl;
        return 1;
    }

    std::string host(argv[1]);
    unsigned short port = atoi(argv[2]);
    std::string nick(argv[3]);

    boost::thread_group thread_group;
    
    User_ptr user(new User(nick));

    int ch = 0;

    initscr();          /* Start curses mode        */
    start_color();          /* Start the color functionality */
    cbreak();           /* Line buffering disabled, Pass on
                         * everty thing to me       */
    keypad(stdscr, TRUE);       /* I need that nifty F1     */
    noecho();
    init_pair(1, COLOR_GREEN, COLOR_BLACK);

    assume_default_colors(COLOR_GREEN, COLOR_BLACK);
    
//    attron(COLOR_PAIR(1));
//    mvprintw(0, 0, "Press End to exit");
//    refresh();
//    attroff(COLOR_PAIR(1));

    InputWindow* wInput = new InputWindow("input", 3, 153, 45, 0);
    ListWindow<User_ptr>* wNames = new ListWindow<User_ptr>("names", 46, 17, 2, 152);
    ContentWindow<Channel_ptr>* wTitle = new ContentWindow<Channel_ptr>("title", 3, 169, 0, 0);
    ListWindow<Message_ptr>* wHistory = new ListWindow<Message_ptr>("main", 44, 153, 2, 0);
   
    // UI is a set of title, history, names and input windows.
    UI_ptr ui(new UI(wTitle, wHistory, wNames, wInput));
    
    ui->names->addRefreshAfterWindow(wInput);
    ui->names->setVisibleSize(44);
    ui->names->setReverseList(false);
    ui->names->addItem(user);
    
    ui->title->addRefreshAfterWindow(wInput);

    ui->history->addRefreshAfterWindow(wInput);
    ui->history->setVisibleSize(42);
 
    IRCSocket server;
    
    while (true) {
        server.server(host, port);
        
        ui->history->addItem(
            Message_ptr(new Message(
                "Connecting to server...",
                Message::SYSTEM
            ))
        );
        try {
            server.connect();
        } catch (std::exception& e) {
            ui->history->addItem(
                Message_ptr(new Message(
                    "Connection failed!",
                    Message::SYSTEM
                    ))
            );
            
            break;
        }

        try {
            server.send(std::string("NICK ").append(user->getNick()));
        } catch (std::exception& e) {
            ui->history->addItem(
                Message_ptr(new Message(
                    "Connection lost.",
                    Message::SYSTEM
                    ))
            );
            
            break;
        }
       
        try {
            server.send(
                std::string("USER ")
                .append(user->getNick())
                .append(" 0 * :")
                .append(user->getName())
            );
        } catch (std::exception& e) {
            ui->history->addItem(
                Message_ptr(new Message(
                    "Connection lost.",
                    Message::SYSTEM
                ))
            );
            
            break;
        }
        
        // start server socket thread to handle data from the server.
        // allows for non-blocking stdin.
        boost::thread* serverSocketThread = new boost::thread(
                &IRCSocket::start, 
                &server, 
                ui, 
                user
        );

        thread_group.add_thread(serverSocketThread);

        break;
    }
    
    bool exit = false;
    std::string line;
    do {
        switch(ch) {   
            case 0: // first run!
                // do nothing here for now.
                break;

            case 263: // backspace
                ui->input->deleteLastChar();
                break;

            case 10: // send message to server!
                line = ui->input->str();
                ui->input->clearInput(); 
                
                if (line.size() == 0) {
                    // empty message; don't do anything
                    break;
                }
                
                if (line.at(0) == '/') {
                    // this is a command!
                    std::string command = line.substr(1, line.find(' ')-1);
                    std::string params;
                    if (line.find(' ') != std::string::npos) {
                        params = line.substr(line.find(' ')+1);
                    }
                    
                    if (command == "nick") {
                        // changing nick.
                        server.send(std::string("NICK ").append(params));
                    } else if (command == "join") {
                        server.send(std::string("JOIN ").append(params));
                    } else if (command == "part") {
                        if (params == "") {
                            // part current channel
                            // check if we are already in a channel
                            if (NULL == ui->getChannel()) {
                                // no channel! 
                                // alert the user that he should join first
                                ui->history->addItem(
                                    Message_ptr(new Message("Can't leave channel"
                                                "- join a channel first!", 
                                                Message::SYSTEM)) 
                                );

                                break;
                            }
                            
                            server.send(
                                std::string("PART ")
                                    .append(ui->getChannel()->getName())
                            );
                        } else {
                            server.send(
                                std::string("PART ")
                                    .append(params));
                        }
                        
                    } else if (command == "topic") {
                        // check if we are already in a channel
                        if (NULL == ui->getChannel()) {
                            // no channel! 
                            // alert the user that he should join first
                            ui->history->addItem(
                                Message_ptr(new Message("Can't send message"
                                            "- join a channel first!", 
                                            Message::SYSTEM))
                            );

                            break;
                        }
                        
                        server.send(
                            std::string("TOPIC ")
                            .append(ui->getChannel()->getName())
                            .append(" :")
                            .append(params)
                        );
                    } else if (command == "quit") {
                        server.send(std::string("QUIT :").append(params));
                    } else if (command == "clear") {
                        ui->history->removeAll();
                    } else if (command == "raw") { // allow sending raw messages to server
                        server.send(params);
                    } else if (command == "debug") { // toggle debug messages
//                        _debug = !_debug;
                    } else if (command == "exit") {
                        // exit client cleanly!
                        exit = true;
                    } else if (command == "server") {
                        // change servers.

                    }
                } else {
                    // this is a message.
                    // check if we are already in a channel
                    if (NULL == ui->getChannel()) {
                        // no channel! 
                        // alert the user that he should join first
                        ui->history->addItem(
                            Message_ptr(new Message("Can't send message - join a channel first!"))
                        );

                        break;
                    }
                    
                    // put it in the channel history list
                    Message_ptr message(new Message(line, user));
                    ui->history->addItem(message);

                    // transmit message to the server
                    server.send(
                        std::string("PRIVMSG ")
                        .append(ui->getChannel()->getName())
                        .append(" :")
                        .append(line)
                    );
                }
                break;
            
            default:
                ui->input->putChar(ch);
                break;
        } 
    } while (!exit && (ch = ui->input->getChar()) != KEY_END);

    // wait for all threads to finish
    thread_group.join_all();

    // end curses mode
    endwin();

    return 0;
}
