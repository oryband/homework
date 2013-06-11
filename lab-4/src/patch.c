#include "util.h"


#define DT_REG 8
#define O_DIRECTORY 00200000

#define SYS_EXIT  1
#define SYS_READ  3
#define SYS_WRITE 4
#define SYS_OPEN  5
#define SYS_CLOSE 6
#define SYS_LSEEK 19
#define SYS_GETDENTS 141

#define EXIT_CODE 0x55

#define BUF_SIZE 1024
#define STDOUT 1
#define STDIN 0


int main (int argc , char* argv[], char* envp[]) {
    int fd_greeting = 0,
        fd_greeting_R = 0,
        pos_greeting = 0,
        i = 5,
        size_newname = 0;

    if ((fd_greeting = system_call(SYS_OPEN, "greeting", 2)) < 0) {
        system_call(SYS_EXIT, EXIT_CODE);
    }

    pos_greeting = system_call(SYS_LSEEK, fd_greeting, 1360, 1);
    fd_greeting_R = system_call(SYS_WRITE, fd_greeting,argv[1], strlen(argv[1])+1);

    return 0;
}
