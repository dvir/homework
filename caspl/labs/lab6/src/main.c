#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <linux/limits.h>
#include <sys/types.h>
#include <sys/wait.h>
#include "../include/LineParser.h"

#define HISTORY_SIZE 10

typedef struct env_variable {
    char* name;
    char* value;
    struct env_variable* next;
} env_variable;

void execute(cmdLine *pCmdLine);

env_variable* env_insert(env_variable* env, char* name, char* value);
env_variable* env_remove(env_variable* env, char* name);
void env_print(env_variable* env);
void env_free(env_variable* env);
char* env_get(env_variable* env, char* name);
char* str_clone(const char* source);

int* sinkPipe(int** pipes, cmdLine* pCmdLine);
int* feedPipe(int** pipes, cmdLine* pCmdLine);
void releasePipes(int** pipes, int nPipes);
int** createPipes(int nPipes);
int countCmds(cmdLine* pCmdLine);

int main (int argc, char** argv) {
    env_variable* env = 0;
    char history[HISTORY_SIZE][2048];
    int historyHead = -1, historyCount = 0, printHead;
    char input[2048];
    char* command;
    char* var_name;
    char* var_value;
    char cwd[PATH_MAX];
    cmdLine* line;
    int ret, i;
    int invokeHistoryIndex;
    char quit = 0;

    getcwd(cwd, PATH_MAX);
    while (!quit) {
        printf("\n%s$ ", cwd);
        fgets(input, 2048, stdin);
        line = parseCmdLines(input);
        command = input;

        if (!line) {
            continue;
        }

        /* go over the arguments looking for an argument that starts with
         * a dollar sign, and replace it if found. */
        for (i = 1; i < line->argCount; ++i) {
            if (line->arguments[i][0] == '$') {
                /* a variable given! search for it in env list */
                var_name = malloc(sizeof(char)*(strlen(line->arguments[i])));
                strcpy(var_name, line->arguments[i]+1);
                var_value = env_get(env, var_name);
                if (strlen(var_value) > 0) {
                    replaceCmdArg(line, i, var_value);
                } else {
                    printf("Couldn't find variable '%s' in the env.", var_name);
                }

                free(var_name);
            }
        }

        if (line->arguments[0][0] == '!') {
            /* invoke history command */
            invokeHistoryIndex = 0;
            if (strlen(line->arguments[0]) > 1) {
                invokeHistoryIndex = line->arguments[0][1] - '0'; 
                if (invokeHistoryIndex > HISTORY_SIZE - 1) {
                    printf("!: Invalid history command index given.\n");
                    continue;
                }
                if (invokeHistoryIndex > historyCount) {
                    printf("!: Out of range history index given.\n");
                    continue;
                }

                /* valid and in range history command index given */
                /* feed input with the history command to make sure we treat
                 * it just as the user entered the command himself */
                printHead = historyHead+1;
                if (historyCount < 10) {
                    printHead = 0;
                }

                command = history[(printHead + invokeHistoryIndex) % HISTORY_SIZE];

                /* free the ! command */
                freeCmdLines(line);
                line = parseCmdLines(command);
            }
        }

        /* add command to history */
        historyHead = (historyHead + 1) % HISTORY_SIZE;
        ++historyCount;
        strcpy(history[historyHead], command);

        if (strcmp(line->arguments[0], "quit") == 0) {
            quit = 1;
        } else if (strcmp(line->arguments[0], "history") == 0) {
            printHead = historyHead+1;
            if (historyCount < 10) {
                printHead = 0;
            }

            for (i = 0; i < historyCount && i < 10; ++i) {
                printf("%d: %s\n", i, history[(i + printHead) % HISTORY_SIZE]);
            }
        } else if (strcmp(line->arguments[0], "set") == 0) {
            if (line->argCount < 3) {
                printf("set: Not enough arguments given.\n"); 
            } else {
                env = env_insert(env, line->arguments[1], line->arguments[2]);
            }
        } else if (strcmp(line->arguments[0], "unset") == 0) {
            if (line->argCount < 2) {
                printf("unset: Not enough arguments given.\n"); 
            } else {
                env = env_remove(env, line->arguments[1]);
            }
        } else if (strcmp(line->arguments[0], "env") == 0) {
            env_print(env);
        } else if (strcmp(line->arguments[0], "cd") == 0) {
            if (line->argCount < 2) {
                /* if no parameters given, assume home directory as target */
                ret = chdir(getenv("HOME"));
            } else {
                ret = chdir(line->arguments[1]);
            }

            if (ret == 0) {
                getcwd(cwd, PATH_MAX);
            } else {
                perror("cd: Failed changing directory. ");
            }
        } else if (strcmp(line->arguments[0], "mygecko") == 0) {
            for (i = 1; i < line->argCount; ++i) {
                printf("%s ", line->arguments[i]);
            }
        } else {
            execute(line);
        }

        freeCmdLines(line);
    }

    env_free(env);
    return 0;
}

void execute(cmdLine *pCmdLine) {
    int ret, status;
    int cmdsCount;
    int** pipes;
    int* cPipe;
    int i;
    cmdLine* cmd = pCmdLine;

    /* calculate amount of pipes needed */
    cmdsCount = countCmds(pCmdLine);
    pipes = createPipes(cmdsCount - 1);
    i = 0;
    while (i < cmdsCount) {
        int cpid = fork();
        if (cpid == 0) {
            /* check if have any pipes in the command */
            if (cmdsCount > 1) {
                if (cmd->idx != 0) {
                    /* if the command is not the first command in the chain,
                     * we should redirect its input to the feed pipe.
                     */

                    /* find the relevant command feed pipe */
                    cPipe = feedPipe(pipes, cmd);
                    
                    /* close stdin */
                    close(0);

                    /* duplicate the relevant pipe's read fd */
                    dup2(cPipe[0], 0);

                    /* close the pipe's read fd */
                    close(cPipe[0]);
                } 
                
                if (cmd->next != NULL) {
                    /* if the command doesn't have a next command,
                     * it's the last command in the chain and therefore
                     * shouldn't redirect its output.
                     * so for each command that is not the last, we should
                     * redirect the output to the relevant pipe.
                     */
                   
                    /* find the relevant command sink pipe */
                    cPipe = sinkPipe(pipes, cmd);
                    
                    /* close stdout */
                    close(1);

                    /* duplicate the relevant pipe's write fd */
                    dup2(cPipe[1], 1);

                    /* close the pipe's write fd */
                    close(cPipe[1]);
                }
            }

            if (cmd->inputRedirect != NULL) {
                close(0);
                fopen(cmd->inputRedirect, "r");
            }

            if (cmd->outputRedirect != NULL) {
                close(1);
                fopen(cmd->outputRedirect, "w");
            }

            ret = execvp(cmd->arguments[0], cmd->arguments);
            if (ret != 0) {
                perror("execute(): failed executing command.");
            }
            exit(0);
            return;
        }

        cPipe = feedPipe(pipes, cmd);
        if (cPipe != NULL) {
            close(cPipe[0]);
        }

        cPipe = sinkPipe(pipes, cmd);
        if (cPipe != NULL) {
            close(cPipe[1]);
        }

        if (cmd->blocking == 1) {
            waitpid(cpid, &status, 0);
        }
        cmd = cmd->next;
        ++i;
    }

    releasePipes(pipes, cmdsCount - 1);
}

int countCmds(cmdLine* pCmdLine) {
    int count = 0;
    cmdLine* it = pCmdLine;

    while (it != NULL) {
        it = it->next;
        count++;
    }

    return count;
}

char* str_clone(const char* source) {
    char* clone = (char*)malloc(strlen(source) + 1);
    strcpy(clone, source);
    return clone;
}

env_variable* env_insert(env_variable* env, char* name, char* value) {
    if (!env) {
        env = malloc(sizeof(env_variable));
        env->name = malloc(sizeof(char)*(strlen(name)+1));
        strcpy(env->name, name);
        env->value = malloc(sizeof(char)*(strlen(value)+1));
        strcpy(env->value, value);
        env->next = 0;
        return env;
    }

    if (strcmp(env->name, name) == 0) {
        /* found variable! update */
        free(env->value);
        env->value = malloc(sizeof(char)*(strlen(value)+1));
        strcpy(env->value, value);
    } else {
        /* didn't find. recurse forward in the list */
        env->next = env_insert(env->next, name, value);
    }

    return env;
}

env_variable* env_remove(env_variable* env, char* name) {
    if (!env) {
        /* no env given. couldn't find variable */
        return env;
    }

    if (strcmp(env->name, name) == 0) {
        /* found variable! return the next of it, so to actually 
         * remove it from the linked list */
        /* free it first! */
        free(env->name);
        free(env->value);
        free(env);
        return env->next;
    } else {
        /* didn't find. recurse forward in the list */
        env->next = env_remove(env->next, name);
    }

    return env;
}

void env_print(env_variable* env) {
    if (env) {
        printf("%s=%s\n", env->name, env->value);
        env_print(env->next);
    }
}

void env_free(env_variable* env) {
    if (env) {
        env_free(env->next);

        free(env->name);
        free(env->value);
        free(env);
    }
}

char* env_get(env_variable* env, char* name) {
    if (!env) {
        return "\0";
    }

    if (strcmp(env->name, name) == 0) {
        return env->value;
    }

    return env_get(env->next, name);
}

int** createPipes(int nPipes) {
    int** pipes = malloc(nPipes * sizeof(int*));
    int* cPipe;
    int i;

    for (i = 0; i < nPipes; ++i) {
        cPipe = malloc(2 * sizeof(int));
        pipe(cPipe);
        pipes[i] = cPipe;
    }

    return pipes;
}

void releasePipes(int** pipes, int nPipes) {
    int i;
    for (i = 0; i < nPipes; ++i) {
        close(pipes[i][0]);
        close(pipes[i][1]);
        free(pipes[i]);
    }

    free(pipes);
}

int* feedPipe(int** pipes, cmdLine* pCmdLine) {
    if (pCmdLine->idx == 0) {
        /* the first command doesn't have a feed pipe */
        return NULL;
    }

    return pipes[pCmdLine->idx - 1];
}

int* sinkPipe(int** pipes, cmdLine* pCmdLine) {
    if (pCmdLine->next == NULL) {
        /* the last command doesn't have a sink pipe */
        return NULL;
    }

    return pipes[pCmdLine->idx];
}
