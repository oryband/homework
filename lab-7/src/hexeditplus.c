#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>

#define ERROR "Error"


void quit(char *f) {
    exit(EXIT_SUCCESS);
}


void memdisplay(char *filename) {
    int address, length,
        i;

    printf("<address> <length>: ");
    scanf("%x %d", &address, &length);
    for (i=0; i<length; i++) {
        printf("%02X ", ((unsigned char *) address)[i]);
    }
    printf("\n");
}


void filedisplay(char *filename) {
    int address, length,
        i;
    FILE *f = fopen(filename, "r");

    if (f == NULL) {
        perror(ERROR);
        exit(EXIT_FAILURE);
    }

    printf("<address> <length>: ");
    scanf("%x %d", &address, &length);

    if (fseek(f, address, SEEK_SET) == -1) {
        perror(ERROR);
        exit(EXIT_FAILURE);
    }

    for (i=0; i<length; i++) {
        printf("%02X ", (unsigned char *) (fgetc(f)));
    }
    printf("\n");
    fclose(f);
}


int main(int argc, char *argv[]) {
    char *labels[]                       = { "Mem Display" , "File Display" , "Quit" };
    void (*functions[]) (char *filename) = { memdisplay    , filedisplay    , quit };
    int i=0,
        choice;

    for (i=0; i < sizeof(labels) / sizeof(labels[0]); i++) {
        printf("[%d]\t%s\n", i+1, labels[i]);
    }
    scanf("%d", &choice);
    functions[choice-1](argv[1]);

    return EXIT_SUCCESS;
}
