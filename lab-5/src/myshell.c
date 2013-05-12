#include <sys/types.h>
#include <sys/wait.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "LineParser.h"


#define BUF_SIZE 2048
#define ERROR "Error"


int execute(cmdLine *pCmdLine) {
    return execvp(pCmdLine->arguments[0], pCmdLine->arguments);
}


int main (int argc, char* argv[]) {
    char wd[BUF_SIZE],
         in[BUF_SIZE];
    cmdLine *cmd;
    pid_t child;
    int i, status;

    while (1) {
        getcwd(wd, BUF_SIZE);
        printf("%s $ ", wd);
        fgets(in, BUF_SIZE, stdin);

        if (strcmp(in, "quit\n") == 0) {
            break;
        }

        cmd = parseCmdLines(in);

        /* Specific cmds */

        if (strcmp(cmd->arguments[0], "cd") == 0) {
            if (cmd->argCount != 2) {
                printf("Bad arguments.\n");
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
