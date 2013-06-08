#include <stdio.h> 
#include <string.h> 

int is_pal (char* str) {
    int length = strlen(str);
    int i;
    for (i = 0; i < length / 2; ++i) {
        if (str[i] != str[length - i - 1]) {
            return 0;
        }
    }

    return 1;
}

int main(int argc, char** argv) {

    return 0;
}
