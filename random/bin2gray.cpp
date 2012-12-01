#include <iostream>
#include <string>
#include <cstdlib>
using namespace std;

int main (int argc, char* argv[]) {
    string gray, binary;
    if (argc == 1) {
        cout << "Enter binary code: ";
        cin >> binary;
    } else {
        binary = "";
        binary.append(argv[1]);
    }

    gray = binary[0];
    for (size_t i = 1; binary[i] != '\0'; ++i) {
        char result = (((binary[i-1] - '0') + (binary[i] - '0')) % 2) + '0';
        gray = gray + result;
    }

    cout << "(" << binary << ")2 == (" << gray << ")gray" << endl;
    return 0;
}
