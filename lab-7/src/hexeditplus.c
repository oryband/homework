#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>


void fun(char *f) {
    exit(EXIT_SUCCESS);
}


int main(int argc, char **argv) {
    char *labels[]                = { "Mem Display" , "File Display" , "Quit" };
    void (*functions[]) (char *f) = { fun           , fun            , fun };
    int i=0,
        choice;

    for (i=0; i < sizeof(labels) / sizeof(labels[0]); i++) {
        printf("[%d]\t%s\n", i, labels[i]);
    }
    scanf("%d", &choice);
    functions[choice](NULL);

    return EXIT_SUCCESS;
}
