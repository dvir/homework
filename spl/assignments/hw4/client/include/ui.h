#ifndef UI_H
#define UI_H

#include "../include/typedef.h"

#include "../include/contentwindow.h"
#include "../include/inputwindow.h"
#include "../include/listwindow.h"

class UI {
    public:
        /**
         * Construct UI from a set of title, history, names and input windows.
         */
        UI(ContentWindow<Channel_ptr>* wTitle, 
           ListWindow<Message_ptr>* wHistory, 
           ListWindow<User_ptr>* wNames, 
           InputWindow* wInput);

	UI(const UI& other);

	UI& operator=(const UI& other);

        ~UI();

        /**
         * Reset the UI by unsetting the active channel
         */
        void reset();

        /**
         * Start a names stream.
         * Initiated when starting to receive /NAMES list
         * of names in a channel.
         */
        void startNamesStream();

        /**
         * End a names stream.
         * Initiated when receiving End of /NAMES list command.
         * Sets the names in the names window.
         */
        void endNamesStream();
        
        /**
         * Add names to current names buffer stream.
         */
        void addNames(std::string str);

        /**
         * Get current channel.
         */
        Channel_ptr getChannel();

        /**
         * Set current channel.
         */
        void setChannel(Channel_ptr newChannel);

        /**
         * Add user to current channel.
         */
        void addUser(User_ptr newUser);

        /**
         * Add a list of users to current channel.
         * Used to avoid refreshing the UI for each
         * user added.
         */
        void addUsers(Users users);

        /** UI title window. **/
        ContentWindow<Channel_ptr>* title;

        /** UI history window. **/
        ListWindow<Message_ptr>* history;

        /** UI names window. **/
        ListWindow<User_ptr>* names;

        /** UI input window. **/
        InputWindow* input;

    private:
        /** Names stream state. **/
        bool _hasStartedNamesStream;

        /** A vector of names list in the names stream. **/
        Strings _namesStream;

        /** Current channel. **/
        Channel_ptr _channel;
};

#endif
