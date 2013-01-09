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

#include "../include/connectionHandler.h"
#include "../include/user.h"
#include "../include/channel.h"
#include "../include/message.h"
#include "../include/network.h"
#include "../include/clientui.h"
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

    std::cout << "Creating connection..." << std::endl;
    ConnectionHandler server(host, port);
    std::cout << "Connecting to server..." << std::endl;
    if (!server.connect()) {
        std::cout << "Cannot connect to " << host << ":" << port << std::endl;
        return 1;
    }
    
    User* user = new User(nick);

    if (!server.send(
                std::string("NICK ")
                .append(user->getNick())
                )
        ) 
    {
        std::cout << "Disconnected. Exiting..." << std::endl;
        return 0;
    }
   
    if (!server.send(
                std::string("USER ")
                .append(user->getNick())
                .append(" 0 * :")
                .append(user->getName())
            )
        ) 
    {
        std::cout << "Disconnected. Exiting..." << std::endl;
        return 0;
    }

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
    ListWindow<User*>* wNames = new ListWindow<User*>("names", 46, 17, 2, 152);
    ContentWindow<Channel*>* wTitle = new ContentWindow<Channel*>("title", 3, 169, 0, 0);
    ListWindow<Message*>* wHistory = new ListWindow<Message*>("main", 44, 153, 2, 0);
   
    // UI is a set of title, history, names and input windows.
    ClientUI* ui = new ClientUI(wTitle, wHistory, wNames, wInput);
    
    ui->names->addRefreshAfterWindow(wInput);
    ui->names->setVisibleSize(44);
    ui->names->setReverseList(false);
    ui->names->addItem(user);
    
    ui->title->addRefreshAfterWindow(wInput);

    ui->history->addRefreshAfterWindow(wInput);
    ui->history->setVisibleSize(42);

//    Utils::debug(ui->history, "*** Starting... ***");

    Network networkRead(&server);
    boost::thread networkInputThread(&Network::read, &networkRead, ui, user);
    
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
                                    new Message("Can't send message - join a channel first!")
                                );

                                break;
                            }
                            
                            server.send(std::string("PART ").append(ui->getChannel()->getName()));
                        } else {
                            server.send(std::string("PART ").append(params));
                        }
                        
                    } else if (command == "topic") {
                        // check if we are already in a channel
                        if (NULL == ui->getChannel()) {
                            // no channel! 
                            // alert the user that he should join first
                            ui->history->addItem(
                                new Message("Can't send message - join a channel first!")
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
                    }
                } else {
                    // this is a message.
                    // check if we are already in a channel
                    if (NULL == ui->getChannel()) {
                        // no channel! 
                        // alert the user that he should join first
                        ui->history->addItem(
                            new Message("Can't send message - join a channel first!")
                        );

                        break;
                    }
                    
                    // put it in the channel history list
                    Message* message = new Message(line, user);
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
    } while((ch = ui->input->getChar()) != KEY_END);

    networkInputThread.join();
    
    // end curses mode
    endwin();

    return 0;
}
