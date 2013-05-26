#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <linux/limits.h>
#include <sys/types.h>
#include <sys/wait.h>

int main (int argc, char** argv) {
    /*
     *
     * Create a pipe.
     * Fork to a child process (child1).
     * On the child1 process:
     *   Close the standard output.
     *   Duplicate the write-end of the pipe using dup (see man).
     *   Close the file descriptor that was duplicated.
     *   Execute "ls -l".
     * On the parent process: Close the write end of the pipe.
     * Fork again to a child process (child2).
     * On the child2 process:
     *   Close the standard input.
     *   Duplicate the read-end of the pipe using dup.
     *   Close the file descriptor that was duplicated.
     *   Execute "tail -n 2".
     * On the parent process: Close the read end of the pipe.
     * Now wait for the child processes to terminate, in the same order of their execution.
     */

    int pipes[2];
    int cpid, cpid2, status;

    char* lsArguments[3] = {"ls", "-al", 0};
    char* tailArguments[4] = {"tail", "-n", "2", 0};

    /* create the new pipe */
    pipe(pipes);

    /* fork first child */
    cpid = fork();
    if (cpid == 0) {
        close(1);
        dup2(pipes[1], 1);
        close(pipes[1]);
        execvp(lsArguments[0], lsArguments);
        return 0;
    }
    /* close the write pipe */
    close(pipes[1]);

    /* fork second child */
    cpid2 = fork();
    if (cpid2 == 0) {
        close(0);
        dup2(pipes[0], 0);
        close(pipes[0]);
        execvp(tailArguments[0], tailArguments);
        return 0;
    }

    /* close the read pipe */
    close(pipes[0]);

    waitpid(cpid, &status, 0);
    waitpid(cpid2, &status, 0);
    return 0;
}
