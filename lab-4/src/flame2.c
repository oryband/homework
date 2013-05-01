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
    printfln(STDOUT, "YOHOOOOO");
    return 0;
}
