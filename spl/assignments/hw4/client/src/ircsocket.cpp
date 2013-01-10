#include "../include/ircsocket.h"

#include "../include/ui.h"
#include "../include/utils.h"
#include "../include/connectionHandler.h"
#include "../include/user.h"
#include "../include/channel.h"
#include "../include/message.h"

#include <string>

IRCSocket::IRCSocket(ConnectionHandler* ch) : 
    _ch(ch)
{
}

IRCSocket::~IRCSocket() {
    delete _ch;
}

void IRCSocket::close() {
    this->_ch->close();
}

void IRCSocket::connect() {
    this->_ch->connect();
}

void IRCSocket::send(std::string message) {
    this->_ch->send(message);
}

void IRCSocket::read(std::string& message) {
    this->_ch->read(message);
}

void IRCSocket::start(UI* ui, User* user) { 
    std::string answer;

    while (true) {
        // read from the socket. NOTE: blocking!
        this->read(answer);

        answer = Utils::trim(answer);
        //        Utils::debug(ui->history, answer);

        if (answer.at(0) == ':') {
            // this is a command!
            Strings data = Utils::split(answer.substr(1), ':');
            Strings params = Utils::split(data[0], ' ');
            std::string host = params[0];
            std::string command = params[1]; 

            std::string target;
            if (params.size() > 2) {
                target = params[2]; 
            }

            std::string msg;
            if (data.size() > 1) {
                // find the second occurrence of ':'
                // and fetch the whole string AFTER it
                msg = answer.substr(answer.find(':', 1)+1);
            }

            // if the one who commited the action is a user,
            // his host will be: nick!user@host
            std::string nick = host.substr(0, host.find('!'));

            if (command == "JOIN") {
                if (nick == user->getNick()) {
                    // joined a new channel!
                    ui->setChannel(new Channel(target));

                    ui->history->addItem(
                            new Message(
                                std::string("Now talking on ").append(target),
                                Message::SYSTEM
                                )
                            );
                } else {
                    // someone else joined a channel we are in
                    // add him to the names list.
                    User* newUser = new User(nick);
                    ui->addUser(newUser);
                    ui->history->addItem(
                            new Message(
                                std::string("has joined ").append(target),
                                newUser,
                                Message::ACTION
                                )
                            );
                }

            } else if (command == "PART") {
                if (nick == user->getNick()) {
                    // quit channel. 
                    ui->setChannel(NULL);

                    ui->history->addItem(
                            new Message(
                                std::string("You have left ").append(target),
                                Message::SYSTEM
                                )
                            );
                } else {
                    // someone quit the current channel.
                    // find him on the names list and remove him.
                    Users users = ui->names->getList();
                    for (size_t ii = 0; ii < users.size(); ++ii) {
                        if (nick == users[ii]->getNick()) {
                            // found him! remove from the list
                            ui->names->removeItem(ii);

                            std::string text = "has left ";
                            text.append(target);
                            if (msg != "") {
                                text.append(" (").append(msg).append(")");
                            }
                            ui->history->addItem(
                                    new Message(
                                        text,
                                        users[ii],
                                        Message::ACTION
                                        )
                                    );

                            break;
                        }
                    }
                }

            } else if (command == "PRIVMSG") {
                if (NULL != ui->getChannel() 
                        && target == ui->getChannel()->getName()) {
                    // message to current channel!
                    ui->history->addItem(
                            new Message(
                                msg,
                                new User(nick)
                                )
                            );
                } else if (target == user->getNick()) {
                    // a private message to the user
                    ui->history->addItem(
                            new Message(
                                msg,
                                new User(nick),
                                Message::PRIVATE 
                                )
                            );
                }

            } else if (command == "QUIT") {
                // someone quit while being in the current channel.
                // find him on the names list and remove him.
                Users users = ui->names->getList();
                for (size_t ii = 0; ii < users.size(); ++ii) {
                    if (nick == users[ii]->getNick()) {
                        // found him! remove from the list
                        ui->names->removeItem(ii);

                        std::string text = "has quit";
                        if (msg != "") {
                            text.append(" (").append(msg).append(")");
                        }
                        ui->history->addItem(
                                new Message(
                                    text,
                                    users[ii],
                                    Message::ACTION
                                    )
                                );

                        break;
                    }
                }
            } else if (command == "NICK") {
                if (nick == user->getNick()) {
                    // current user changed nick!
                    // update current user
                    user->setNick(msg);
                } else {
                    // someone else changed nick.
                    // find him in channel list and update his nick.
                    Users users = ui->names->getList();
                    for (size_t ii = 0; ii < users.size(); ++ii) {
                        if (nick == users[ii]->getNick()) {
                            // found the user. change his nick
                            // to the new nick.
                            users[ii]->setNick(msg);
                            ui->names->redraw();

                            ui->history->addItem(
                                    new Message(
                                        std::string("is now known as ")
                                        .append(msg),
                                        users[ii],
                                        Message::ACTION
                                        )
                                    );

                            break;
                        }
                    }
                }

            } else if (command == "NOTICE") {
                if (NULL != ui->getChannel() && 
                        target == ui->getChannel()->getName()) {
                    // message to current channel!
                    ui->history->addItem(
                            new Message(
                                msg,
                                new User(nick)
                                )
                            );
                } else if (target == user->getNick()) {
                    // a private notice to the user
                    ui->history->addItem(
                            new Message(
                                msg,
                                new User(nick),
                                Message::PRIVATE 
                                )
                            );
                }

            } else if (command == "332" || command == "TOPIC") {
                // topic change
                // changing topic 
                ui->getChannel()->setTopic(msg); 
                ui->title->redraw();

                ui->history->addItem(
                        new Message(
                            std::string("has changed the topic to: ")
                            .append(msg),
                            new User(nick),
                            Message::ACTION
                            )
                        );
            } else if (command == "353") { // names list
                // this will start a names stream 
                // only if there is none active
                ui->startNamesStream();
                ui->addNames(msg);

            } else if (command == "366") { // end names list
                // stop streaming names to the ui.
                // this will set the names list.
                ui->endNamesStream();

            } else if (command == "001"
                    || command == "002"
                    || command == "003"
                    || command == "004"
                    || command == "005"
                    || command == "251"
                    || command == "252"
                    || command == "254"
                    || command == "255"
                    || command == "372"
                    || command == "372"
                    || command == "375"
                    || command == "376"
                    ) {
                ui->history->addItem(
                        new Message(
                            msg,
                            Message::SYSTEM
                            )
                        );
            }
        } else {
            // this is a command!
            Strings params = Utils::split(answer, ':');
            std::string command = Utils::trim(params[0]); 

            std::ostringstream response;
            if (command == "PING") {
                response << "PONG :" << params[1];
                _ch->send(response.str());
            } else if (command == "NOTICE AUTH") {
                ui->history->addItem(
                        new Message(
                            params[1],
                            Message::SYSTEM
                            )
                        );
            }
        }

        answer.clear();
    }
}
