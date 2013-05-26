#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>

#include "LineParser.h"

#define STDIN  0
#define STDOUT 1

#define BUF_SIZE 2048

#define ERROR "Error"


typedef struct {
    char name[BUF_SIZE], value[BUF_SIZE];
    struct envVars *next;
} envVars;


int execute(cmdLine *pCmdLine) {
    int in, out;

    if (pCmdLine->inputRedirect != NULL) {
        close(STDIN);
        in = open(pCmdLine->inputRedirect, O_RDONLY, S_IRUSR | S_IRGRP | S_IROTH);
        if (in == -1) {
            printf("Error: open(), inputRedirect.\n");
            return EXIT_FAILURE;
        }
    }

    if (pCmdLine->outputRedirect != NULL) {
        close(STDOUT);
        out = open(pCmdLine->outputRedirect, O_WRONLY, S_IWUSR | S_IWGRP | S_IWOTH);
        if (out == -1) {
            printf("Error: open(), outputRedirect.\n");
            return EXIT_FAILURE;
        }
    }

    return execvp(pCmdLine->arguments[0], pCmdLine->arguments);
}


int main (int argc, char* argv[]) {
    cmdLine *cmd;
    pid_t child;
    /*
     * TODO: Lab 5, task 2.
     * envVars *envRoot = NULL,
     *         *env = NULL;
     */
    int i,j,
        h=0, hIndex,
        status;
    char wd[BUF_SIZE],
         in[BUF_SIZE],
         history[10][BUF_SIZE] = { "", "", "", "", "", "", "", "", "", ""};

    while (1) {
        getcwd(wd, BUF_SIZE);
        printf("%s $ ", wd);
        fgets(in, BUF_SIZE, stdin);

        /* Pre-checks. */

        if (strcmp(in, "quit\n") == 0) {
            break;
        } else if (in[0] == '!') {
            if (in[1] == 0) {
                printf("%s: No index.", ERROR);
            } else {
                hIndex = (atoi(&in[1]) +h-1) %10;
                if (hIndex >= 10 || strcmp(history[hIndex], "") == 0) {
                    printf("%s: Bad index.\n", ERROR);
                    continue;
                } else {
                    strcpy(in, history[hIndex]);
                }
            }
        }

        cmd = parseCmdLines(in);

        if (h == 10) {
            h=0;
        }
        strcpy(history[h++], in);

        /* Specific cmds */

        if (strcmp(cmd->arguments[0], "cd") == 0) {
            if (cmd->argCount != 2) {
                printf("%s: Bad arguments.\n", ERROR);
            } else {
                status = chdir(cmd->arguments[1]);
                if (status != 0) {
                    perror(ERROR);
                }
            }
        } else if (strcmp(cmd->arguments[0], "mygecko") == 0) {
            for (i=0; i < cmd->argCount -1; i++) {
                printf("%s ", cmd->arguments[i+1]);
            }
            printf("\n");
        } else if (strcmp(cmd->arguments[0], "history") == 0) {
            if (strcmp(history[h], "") == 0) {
                j = 0;
            } else {
                j = h;
            }

            for (i=j; i < j+10; i++) {
                if (strcmp(history[i%10], "") == 0) {
                    break;
                }

                printf("%d:\t%s", i-j+1, history[i%10]);
            }

        /*
         * TODO Lab 5, task 2.
         * } else if (strcmp(cmd->arguments[0], "set") == 0) {
         *     if (cmd->argCount != 3) {
         *         printf("%s: Bad arguments.", ERROR);
         *     } else {
         *         if (envRoot == NULL) {
         *             envRoot = (env *) malloc(sizeof(env));
         *             env->name  = strcpy(cmd->arguments[1]);
         *             env->value = strcpy(cmd->arguments[2]);
         *             env->next = NULL:
         *         } else {
         *             env = envRoot->next;
         *             while (env->next != NULL) {
         *                 env = env->next;
         *             }
         *             TODO: Continue here...
         *         }
         */

        /* Other cmds */

        } else {
            child = fork();
            if (child == 0) {
                status = execute(cmd);
                if (status != EXIT_SUCCESS) {
                    perror(ERROR);
                    _exit(status);
                }
            } else {
                if ((int) cmd->blocking) {
                    waitpid(child, &status, 0);
                }
            }
        }

        freeCmdLines(cmd);
        printf("\n");
    }

    return EXIT_SUCCESS;
}
