#include <sys/types.h>
#include <sys/wait.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "LineParser.h"


#define BUF_SIZE 2048


int execute(cmdLine *pCmdLine) {
    return execvp(pCmdLine->arguments[0], pCmdLine->arguments);
}


int main (int argc, char* argv[]) {
    char wd[BUF_SIZE],
         in[BUF_SIZE];
    cmdLine *cmd;
    pid_t child;
    int status;

    while (1) {
        getwd(wd);
        printf("%s $ ", wd);
        fgets(&in, BUF_SIZE, stdin);

        if (strcmp(in, "quit\n") == 0) {
            break;
        }

        cmd = parseCmdLines(in);
        child = fork();
        if (child == 0) {
            status = execute(cmd);
            if (status != EXIT_SUCCESS) {
                perror("Error");
                _exit(status);
            } else {
                _exit(EXIT_SUCCESS);
            }
        } else {
            if ((int) cmd->blocking) {
                waitpid(child, &status, 0);
            }
            freeCmdLines(cmd);
            printf("\n");
        }
    }

    return 0;
}
