#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <linux/limits.h>
#include <sys/types.h>
#include <sys/wait.h>

int main (int argc, char** argv) {
    int status, cpid, cpid2;
    int pipes[2];

    char* lsArguments[] = {"ls", "-l", NULL};
    char* tailArguments[] = {"tail", "-n", "2", NULL};

    pipe(pipes);
    cpid = fork();
    if (cpid == 0) {
        close(1);
        dup2(pipes[1], 1);
        close(pipes[1]);
        execvp(lsArguments[0], lsArguments);
        return 0;
    }
    /* close write pipe */
    close(pipes[1]);

    cpid2 = fork();
    if (cpid2 == 0) {
        close(0);
        dup2(pipes[0], 0);
        close(pipes[0]);
        execvp(tailArguments[0], tailArguments);
        return 0;
    }
    /* close read pipe */
    close(pipes[0]);

    waitpid(cpid, &status, 0);
    waitpid(cpid2, &status, 0);
    return 0;
}
