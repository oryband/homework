#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "LineParser.h"


#define FGETS_SIZE 2048


int execute(cmdLine *pCmdLine) {
    return execvp(pCmdLine->arguments[0], pCmdLine->arguments);
}


int main (int argc, char* argv[]) {
    char wd[FGETS_SIZE],
         in[FGETS_SIZE];
    cmdLine *cmd;

    while (1) {
        getwd(wd);
        printf("%s $ ", wd);
        fgets(&in, FGETS_SIZE, stdin);

        if (strcmp(in, "quit\n") == 0) {
            break;
        }

        cmd = parseCmdLines(in);
        if (execute(cmd) == -1) {
            perror("Error");
        }
        freeCmdLines(cmd);
        printf("\n");
    }

    return 0;
}
