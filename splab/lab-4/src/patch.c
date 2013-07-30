#include "util.h"
#include "task2.h"

#define SYS_EXIT  1
#define SYS_READ  3
#define SYS_WRITE 4
#define SYS_OPEN  5
#define SYS_CLOSE 6
#define SYS_LSEEK 19

#define STDOUT 1

#define O_RDONLY 0
#define O_WRONLY 1
#define O_CREAT 64

#define SEEK_SET 0

int main (int argc , char* argv[], char* envp[]) {
    char *name,
         read_buf;
    int rfd, wfd;

    if (argc < 2) {
        system_call(SYS_WRITE, STDOUT, "Error! Not enough arguments.\n", 28);
        system_call(SYS_EXIT, 0x55, 0, 0);
    }

    name = argv[1];

    rfd = system_call(SYS_OPEN, "bin/greeting", O_RDONLY, 0);
    if (rfd < 0) {
        system_call(SYS_WRITE, STDOUT, "Error! Open read.", 15);
        system_call(SYS_EXIT, 0x55, 0, 0);
    }

    wfd = system_call(SYS_OPEN, "bin/greeting2", O_WRONLY | O_CREAT, 0777);
    if (wfd < 0) {
        system_call(SYS_WRITE, STDOUT, "Error! Open write.", 16);
        system_call(SYS_EXIT, 0x55, 0, 0);
    }

    while (system_call(SYS_READ, rfd, &read_buf, 1)) {
        system_call(SYS_WRITE, wfd, &read_buf, 1);
    }

    system_call(SYS_LSEEK, wfd, 0x550, SEEK_SET);
    system_call(SYS_WRITE, wfd, name, strlen(name) +1);

    system_call(SYS_CLOSE, wfd, 0, 0);
    system_call(SYS_CLOSE, rfd, 0, 0);

    return 0;
}
