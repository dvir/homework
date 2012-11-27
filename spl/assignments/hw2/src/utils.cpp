#include "../include/utils.h"
#include <fstream>
#include <sstream>
using namespace std;

vector<string> Utils::str_split(string str, char separator) {
	vector<string> words;
	string word = ""; 
    string::iterator it;
	for (it = str.begin(); it < str.end(); ++it) {
		if (*it == separator) {
			words.push_back(word);
			word = "";
		} else {
			word = word + *it;
		}
	}

	if (word != "") words.push_back(word);
	
	return words;
}

void Utils::log(string str) {
    ofstream log;
    log.open("random.log", ios::app);
    if (!log.is_open()) {
        throw ("Unable to open log file.");
    }
    log << str << std::endl;
    log.close();
}

void Utils::log(size_t num, string str) {
    stringstream ss;
    ss << num << str;
    log(ss.str());
}

void Utils::log(string str1, size_t num, string str2) {
    stringstream ss;
    ss << str1 << num << str2;
    log(ss.str());
}

void Utils::log(size_t num, string str1, string str2) {
    stringstream ss;
    ss << num << str1 << str2;
    log(ss.str());
}

void Utils::log(size_t num, string str1, string str2, string str3) {
    stringstream ss;
    ss << num << str1 << str2 << str3;
    log(ss.str());
}
