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

int main (int argc, char** argv) {
    env_variable* env;
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
                    /* replace it with the new value */
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
    int ret, status, cpid;

    cpid = fork();
    if (cpid == 0) {
        /* check for input redirection */
        if (pCmdLine->inputRedirect) {
            /* close stdin */
            close(0);
            fopen(pCmdLine->inputRedirect, "r");
        }

        /* check for output redirection */
        if (pCmdLine->outputRedirect) {
            /* close stdout */
            close(1);
            fopen(pCmdLine->outputRedirect, "w");
        }

        ret = execvp(pCmdLine->arguments[0], pCmdLine->arguments);
        if (ret != 0) {
            perror("execute(): failed executing command.");
            _exit(0);
        }
    }

    if (pCmdLine->blocking == 1) {
        waitpid(cpid, &status, 0);
    }
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
