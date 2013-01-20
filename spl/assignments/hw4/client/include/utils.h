#ifndef UTILS_H
#define UTILS_H

#include "../include/typedef.h"

#include <string>
#include <vector>

class Utils {
    public:
        /**
         * Trim string from left.
         */
        static std::string &ltrim(std::string &s);

        /**
         * Trim string from right.
         */
        static std::string &rtrim(std::string &s);

        /**
         * Trim string from both left and right.
         */
        static std::string &trim(std::string &s);

        /**
         * Wordwrap a @param str (insert \n) every @param width characters.
         * @param std::string str String to wordwrap.
         * @param size_t width    Width to wordwrap it to.
         * @return std::string Wordwrapped string.
        **/
        static std::string wordwrap(const std::string &str, size_t width);

        /**
         * Split a string into a vector of strings by a delimiter.
         */
        static std::vector<std::string> split(const std::string &s, char delim);
        static std::vector<std::string> &split(const std::string &s, char delim, std::vector<std::string> &elems);

        /**
         * Find the index of the nth character of a kind in a string.
         */
        static size_t find_nth(const std::string &s, const std::string &delim, unsigned long n);

        /**
         * Collapse multiple spaces into one space.
         */
        static std::string collapseMultipleSpaces(const std::string &s);
};

#endif
