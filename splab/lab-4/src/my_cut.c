#include "util.h"

#define SYS_READ  3
#define SYS_WRITE 4
#define SYS_OPEN  5
#define SYS_CLOSE 6

#define STDOUT 1
#define STDIN 2


typedef int FILE;


void printf(int fd, char *msg) {
    system_call(SYS_WRITE, fd, msg, strlen(msg));
}

void printcf(int fd, char *msg) {
    system_call(SYS_WRITE, fd, msg, 1);
}

void newlinef(int fd) {
    system_call(SYS_WRITE, fd, "\n", 1);
}

void printlnf(int fd, char *msg) {
    printf(fd, msg);
    newlinef(fd);
}


int fopen(char *path) {
    return system_call(SYS_OPEN, path, 0, 0);
}

int fclose(FILE f) {
    return system_call(SYS_CLOSE, f);
}


int main (int argc, char* argv[], char* envp[]) {
    int i, filesLen, k;
    FILE in, ins[3]={ STDIN, 0, 0 };
    char s[1], *option;

    if (argc < 2 || argc > 5) {
        printlnf(STDOUT, "Missing arguments.");
        return 1;
    }

    option = argv[1];

    for (i=2; i < argc; i++) {
        if (filesLen >= 3) {
            printlnf(STDOUT, "Error! Output files should be <= 3.");
            return 1;
        }

        if (strcmp("stdin", argv[i]) != 0) {
            ins[filesLen++] = fopen(argv[i]);
        }
    }

    for (k=0; k < filesLen; k++) {
        if (ins[k] != 0) {
            i=1;
            while (system_call(SYS_READ, ins[k], s, 1) != 0) {
                if (s[1] == '\n') {
                    i=1;
                    newlinef(STDOUT);
                    newlinef(STDOUT);
                    continue;
                }

                if (strcmp(option, "-o") == 0) {
                    if (i%2 == 1) {
                        printcf(STDOUT, s);
                    }
                } else if (strcmp(option, "-e") == 0) {
                    if (i%2 == 0) {
                        printcf(STDOUT, s);
                    }
                } else {
                    printcf(STDOUT, s);
                }

                i++;
            }
        }
    }

    for (k=0; k < filesLen; k++) {
        in = ins[i];
        if (in != STDIN && in != 0) {
            fclose(in);
        }
    }

    newlinef(STDOUT);
    return 0;
}
