#include <iostream>
#include <string>
#include <cstdlib>
using namespace std;

int main (int argc, char* argv[]) {
    string gray, binary;
    if (argc == 1) {
        cout << "Enter gray code: ";
        cin >> gray;
    } else {
        gray = "";
        gray.append(argv[1]);
    }

    binary = gray[0];
    for (size_t i = 1; gray[i] != '\0'; ++i) {
        char result = (((binary[i-1] - '0') + (gray[i] - '0')) % 2) + '0';
        binary = binary + result;
    }

    cout << "(" << gray << ")gray == (" << binary << ")2" << endl;
    return 0;
}
