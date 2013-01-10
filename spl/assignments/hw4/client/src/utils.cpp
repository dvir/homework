#include "../include/typedef.h"
#include "../include/utils.h"

#include "../include/message.h"
#include "../include/user.h"

#include <sstream>
#include <ostream>
#include <istream>
#include <iostream>
#include <algorithm> 
#include <functional> 
#include <cctype>
#include <locale>

// trim from start
std::string& Utils::ltrim(std::string &s) {
     s.erase(s.begin(), std::find_if(s.begin(), s.end(), std::not1(std::ptr_fun<int, int>(std::isspace))));
     return s;
 }

// trim from end
std::string& Utils::rtrim(std::string &s) {
     s.erase(std::find_if(s.rbegin(), s.rend(), std::not1(std::ptr_fun<int, int>(std::isspace))).base(), s.end());
     return s;
}

// trim from both ends
std::string& Utils::trim(std::string &s) {
     return ltrim(rtrim(s));
}

/**
 * Wordwrap a @param str (insert \n) every @param width characters.
 * @param std::string str String to wordwrap.
 * @param size_t width    Width to wordwrap it to.
 * @return std::string Wordwrapped string.
**/
std::string Utils::wordwrap(const std::string& str, size_t width) {
    std::string newString(str);
    for (size_t ii = width; ii < newString.size(); ii += width) {
        newString.insert(ii, "\n");
    }

    return newString;
}

std::vector<std::string>& Utils::split(const std::string &s, char delim, std::vector<std::string> &elems) {
    std::stringstream ss(s);
    std::string item;
    while(std::getline(ss, item, delim)) {
        elems.push_back(item);
    }

    return elems;
}

std::vector<std::string> Utils::split(const std::string &s, char delim) {
    std::vector<std::string> elems;
    return split(s, delim, elems);
}

//void Utils::debug (ListWindow<Message*>* history, std::string message) {
//    if (true) {
//        return;
//    }
//
//    history->addItem(new Message(message, new User("debug"), Message::DEBUG));
//}
