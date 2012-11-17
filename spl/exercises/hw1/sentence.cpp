#include <iostream>
#include <string>
#include <vector>
using namespace std;

vector<string> split(char sentence[], int len) {
	vector<string> words;
	string word = ""; 
	for (size_t i = 0; i < len; i++) {
		if (sentence[i] == ' ') {
			words.push_back(word);
			word = "";
		} else {
			word = word + sentence[i];
		}
	}
	if (word != "") words.push_back(word);
	
	return words;
}

void printWords(vector<string> words) {
	for (size_t i = 0; i < words.size(); i++) {
		cout << words[i] << endl;
	}
}

int main(int argc, char *argv[]) {
	char sentence[15] = "THIS IS SPARTA";
	printWords(split(sentence, sizeof(sentence)));
	return 0;
} 
