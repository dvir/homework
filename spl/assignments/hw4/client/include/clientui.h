#ifndef CLIENTUI_H
#define CLIENTUI_H

#include "../include/typedef.h"
#include "../include/utils.h"
#include "../include/contentwindow.h"
#include "../include/inputwindow.h"
#include "../include/listwindow.h"

class ClientUI {
    public:
        ClientUI(ContentWindow<Channel*>* wTitle, 
           ListWindow<Message*>* wHistory, 
           ListWindow<User*>* wNames, 
           InputWindow* wInput) :
            title(wTitle),
            history(wHistory),
            names(wNames),
            input(wInput),
            _hasStartedNamesStream(false)
        {
        }

        virtual ~ClientUI()
        {
        }

        virtual void startNamesStream() {
            if (false == _hasStartedNamesStream) {
                _hasStartedNamesStream = true;
            }
        };

        virtual void endNamesStream() {
            _hasStartedNamesStream = false;

            if (_namesStream.size() > 0) {
                this->names->removeAll();

                Users users;
                for (Strings::iterator it = _namesStream.begin();
                     it != _namesStream.end();
                     ++it)
                {
                    User* newUser = new User(*it);
                    users.push_back(newUser);
                }

                sort(users.begin(), users.end(), UserPointerCompare);
                this->addUsers(users);
                _namesStream.clear();
            }
        };

        virtual void addNames(std::string str) {
            Strings names = Utils::split(str, ' ');
            for (Strings::iterator it = names.begin();
                 it != names.end();
                 ++it)
            {
                _namesStream.push_back(*it);
            }
        };

        virtual Channel* getChannel() {
            return _channel;
        };

        virtual void setChannel(Channel* newChannel) {
            _channel = newChannel;
            this->title->setContent(_channel);
            this->names->removeAll();
        };

        virtual void addUser(User* newUser) {
            this->names->addItem(newUser);

            if (NULL != this->getChannel()) {
                this->getChannel()->addUser(newUser);
            }
        };

        virtual void addUsers(Users users) {
            this->names->addItems(users);
            
            if (NULL != this->getChannel()) {
                this->getChannel()->addUsers(users);
            }
        };

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
