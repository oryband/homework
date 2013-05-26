#include <sys/types.h>
#include <sys/wait.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>


#define STDIN  0
#define STDOUT 1

#define BUF_SIZE 1024


int main (int argc, char* argv[]) {
    int fildes[2], fildesCopy[2];
    char *lsArgs[]   = { "ls", "-l", 0 },
         *tailArgs[] = { "tail", "-n", "2", 0 };
    pid_t child1, child2;

    if (pipe(fildes) != 0) {
        printf("Error: pipe().\n");
        exit(EXIT_FAILURE);
    }

    child1 = fork();
    if (child1 == 0) {
        if (close(STDOUT) == -1) {
            printf("Error: close(STDOUT).\n");
            exit(EXIT_FAILURE);
        }
        if ((fildesCopy[1] = dup(fildes[1])) == -1) {
            printf("Error: dup(fildes[1]).\n");
            exit(EXIT_FAILURE);
        }
        if (close(fildes[1]) == -1) {
            printf("Error: close(fildes[1]).\n");
            exit(EXIT_FAILURE);
        }
        if (execvp(lsArgs[0], lsArgs) == -1) {
            printf("Error: execvp(ls -l).\n");
            exit(EXIT_FAILURE);
        }
    } else {
        if (close(fildes[1]) == -1) {
            printf("Error: parent close(fildes[1]).\n");
            exit(EXIT_FAILURE);
        }
    }

    child2 = fork();
    if (child2 == 0) {
        if (close(STDIN) == -1) {
            printf("Error: close(STDIN).\n");
            exit(EXIT_FAILURE);
        }
        if ((fildesCopy[0] = dup(fildes[0])) == -1) {
            printf("Error: dup(fildes[0]).\n");
            exit(EXIT_FAILURE);
        }
        if (close(fildes[0]) == -1) {
            printf("Error: close(fildes[0]).\n");
            exit(EXIT_FAILURE);
        }
        if (execvp(tailArgs[0], tailArgs) == -1) {
            printf("Error: execvp(tail -n 2).\n");
            exit(EXIT_FAILURE);
        }
    } else {
        if (close(fildes[0]) == -1) {
            printf("Error: parent close(fildes[0]).\n");
            exit(EXIT_FAILURE);
        }

        printf("Waiting...\n");
        waitpid(child1, 0, 0);
        printf("Child1 finished.");
        waitpid(child2, 0, 0);
        printf("Child2 finished.\n");
    }

    return EXIT_SUCCESS;
}
