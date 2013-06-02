#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>

#define BUF_SIZE 1024
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
    int offset, length,
        i;
    FILE *f = fopen(filename, "r");

    if (f == NULL) {
        perror(ERROR);
        exit(EXIT_FAILURE);
    }

    printf("<offset> <length>: ");
    scanf("%x %d", &offset, &length);

    if (fseek(f, offset, SEEK_SET) == -1) {
        perror(ERROR);
        exit(EXIT_FAILURE);
    }

    for (i=0; i<length; i++) {
        printf("%02X ", (unsigned char *) (fgetc(f)));
    }
    printf("\n");
    fclose(f);
}


void filemodify(char *filename) {
    int offset, value;
    FILE *f = fopen(filename, "r+");

    if (f == NULL) {
        perror(ERROR);
        exit(EXIT_FAILURE);
    }

    printf("<offset> <value>: ");
    scanf("%x %x", &offset, &value);

    if (fseek(f, offset, SEEK_SET) == -1) {
        perror(ERROR);
        exit(EXIT_FAILURE);
    }

    fputc(value, f);
    fclose(f);
}


void copyfromfile(char *filename) {
    int start, target, length,
        i;

    char source[BUF_SIZE];
    FILE *f, *s;

    printf("<source> <s> <t> <length>: ");
    scanf("%s %x %x %d", source, &start, &target, &length);

    if ((f = fopen(filename, "r+")) == NULL) {
        perror(ERROR);
        exit(EXIT_FAILURE);
    }
    if ((s = fopen(source, "r")) == NULL) {
        perror(ERROR);
        exit(EXIT_FAILURE);
    }

    if (fseek(s, start, SEEK_SET) == -1) {
        perror(ERROR);
        exit(EXIT_FAILURE);
    }
    if (fseek(f, target, SEEK_SET) == -1) {
        perror(ERROR);
        exit(EXIT_FAILURE);
    }

    for (i=0; i<length; i++) {
        fputc(fgetc(s), f);
    }

    fclose(f);
    fclose(s);
}


int main(int argc, char *argv[]) {
    char *labels[]                       = { "Mem Display" , "File Display" , "File Modify" , "Copy From File" , "Quit" };
    void (*functions[]) (char *filename) = { memdisplay    , filedisplay    , filemodify    , copyfromfile     , quit };
    int i=0,
        choice;

    for (i=0; i < sizeof(labels) / sizeof(labels[0]); i++) {
        printf("[%d]\t%s\n", i+1, labels[i]);
    }
    scanf("%d", &choice);
    functions[choice-1](argv[1]);

    return EXIT_SUCCESS;
}
