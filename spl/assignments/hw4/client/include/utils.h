#ifndef UTILS_H
#define UTILS_H

#include "../include/typedef.h"

#include <string>
#include <vector>

class Utils {
    public:
        // trim from start
        static std::string &ltrim(std::string &s);

        // trim from end
        static std::string &rtrim(std::string &s);

        // trim from both ends
        static std::string &trim(std::string &s);

        /**
         * Wordwrap a @param str (insert \n) every @param width characters.
         * @param std::string str String to wordwrap.
         * @param size_t width    Width to wordwrap it to.
         * @return std::string Wordwrapped string.
        **/
        static std::string wordwrap(const std::string& str, size_t width);

        static std::vector<std::string> &split(const std::string &s, char delim, std::vector<std::string> &elems);

        static std::vector<std::string> split(const std::string &s, char delim);
};

#endif
