#ifndef TYPEDEF_H
#define TYPEDEF_H

#include <string>
#include <vector>
#include <boost/shared_ptr.hpp>

class User;
class Channel;
class Message;
class Window;
class InputWindow;
class Utils;
class UI;
class ConnectionHandler;

typedef std::vector<std::string> Strings;
typedef std::vector<Window*> Windows;

typedef boost::shared_ptr<User> User_ptr;
typedef boost::shared_ptr<Channel> Channel_ptr;
typedef boost::shared_ptr<Message> Message_ptr;
typedef boost::shared_ptr<UI> UI_ptr;

typedef std::vector<User_ptr> Users;
typedef std::vector<Channel_ptr> Channels;
typedef std::vector<Message_ptr> Messages;

#endif
