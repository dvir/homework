#include "../include/util.h"
#include "../include/task2.h"

#define SYS_WRITE 4
#define SYS_OPEN 5
#define SYS_CLOSE 6
#define SYS_READ 3
#define SYS_LSEEK 19
#define SYS_EXIT 1
#define GET_DENTS 141
#define STDIN 0
#define STDOUT 1
#define STDERR 2

#define O_RDONLY 0
#define O_WRONLY 1
#define O_CREAT 64
#define O_APPEND 1024

#define SEEK_SET 0

int main (int argc , char* argv[], char* envp[]) {
    char* name;
    char read_buf;
    int rfd, wfd;

    if (argc < 2) {
        system_call(SYS_WRITE, STDOUT, "fail! not enough arguments\n", 28);
        system_call(SYS_EXIT, 0x55, 0, 0);
    }

    name = argv[1];

    rfd = system_call(SYS_OPEN, "greeting", O_RDONLY, 0);
    if (rfd < 0) {
        system_call(SYS_WRITE, STDOUT, "fail! open read", 15);
        system_call(SYS_EXIT, 0x55, 0, 0);
    }

    /* copy greeting into greeting2 */
    wfd = system_call(SYS_OPEN, "greeting2", O_WRONLY | O_CREAT, 0777);
    if (wfd < 0) {
        system_call(SYS_WRITE, STDOUT, "fail! open write", 16);
        system_call(SYS_EXIT, 0x55, 0, 0);
    }

    while (system_call(SYS_READ, rfd, &read_buf, 1)) {
        system_call(SYS_WRITE, wfd, &read_buf, 1);
    }

    system_call(SYS_LSEEK, wfd, 0x550, SEEK_SET);
    system_call(SYS_WRITE, wfd, name, strlen(name)+1);

    system_call(SYS_CLOSE, wfd, 0, 0);
    system_call(SYS_CLOSE, rfd, 0, 0);
    return 0;
}
