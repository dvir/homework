#ifndef UI_H
#define UI_H

#include "../include/typedef.h"

#include "../include/contentwindow.h"
#include "../include/inputwindow.h"
#include "../include/listwindow.h"

class UI {
    public:
        UI(ContentWindow<Channel_ptr>* wTitle, 
           ListWindow<Message_ptr>* wHistory, 
           ListWindow<User_ptr>* wNames, 
           InputWindow* wInput);

        ~UI();

        /**
         * Reset the UI by unsetting the active channel
         */
        void reset();

        void startNamesStream();

        void endNamesStream();
        
        void addNames(std::string str);

        Channel_ptr getChannel();

        void setChannel(Channel_ptr newChannel);

        void addUser(User_ptr newUser);

        void addUsers(Users users);

        ContentWindow<Channel_ptr>* title;
        ListWindow<Message_ptr>* history;
        ListWindow<User_ptr>* names;
        InputWindow* input;

    private:
        bool _hasStartedNamesStream;
        Strings _namesStream;
        Channel_ptr _channel;
};

#endif
