#ifndef UI_H
#define UI_H

#include "../include/typedef.h"

#include "../include/contentwindow.h"
#include "../include/inputwindow.h"
#include "../include/listwindow.h"

class UI {
    public:
        UI(ContentWindow<Channel*>* wTitle, 
           ListWindow<Message*>* wHistory, 
           ListWindow<User*>* wNames, 
           InputWindow* wInput);

        ~UI();

        void startNamesStream();

        void endNamesStream();
        
        void addNames(std::string str);

        Channel* getChannel();

        void setChannel(Channel* newChannel);

        void addUser(User* newUser);

        void addUsers(Users users);

        ContentWindow<Channel*>* title;
        ListWindow<Message*>* history;
        ListWindow<User*>* names;
        InputWindow* input;

    private:
        bool _hasStartedNamesStream;
        Strings _namesStream;
        Channel* _channel;
};

#endif
