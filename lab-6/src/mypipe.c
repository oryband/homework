#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define BUF_SIZE 2048

#define STDOUT 1


int main (int argc, char* argv[]) {
    int fildes[2];
    char inMsg[] = "hello",
         outMsg[BUF_SIZE];
    pid_t child;

    if (pipe(fildes) != 0) {
        printf("Error: Pipe creation.\n");
        return -1;
    }

    child = fork();
    if (child == 0) {
        if (write(fildes[1], inMsg, strlen(inMsg)) == -1) {
            printf("Error: write.\n");
            return -1;
        }
    } else {
        waitpid(child, 0, 0);
        if (read(fildes[0], outMsg, BUF_SIZE) == -1) {
            printf("Error: read.\n");
            return -1;
        }
        if (write(STDOUT, outMsg, strlen(outMsg) -1) == -1) {
            printf("Error: write.\n");
            return -1;
        }
        printf("\n");
    }

    return 0;
}
