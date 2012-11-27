#ifndef UTILS_H
#define UTILS_H
#include <string>
#include <vector>
using namespace std;
class Utils {
    public:
        static vector<string> str_split(string str, char separator);
        static void log(string str);
        static void log(size_t num, string str);
        static void log(string str1, size_t num, string str2);
        static void log(size_t num, string str1, string str2);
        static void log(size_t num, string str1, string str2, string str3);
};
#endif
