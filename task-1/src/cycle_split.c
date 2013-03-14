#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>


int main(int argc, char **argv) {
    FILE *out=stdout,
         *in=stdin;
    char c;
    char *d = "W";
    int num, counter;

    puts("---");

    if (argc == 3 && strcmp(argv[1], "-d") == 0) {
        d = argv[2];
    }

    if (strcmp(d, "W") == 0) {
        while ((c = fgetc(in)) != EOF) {
            if (isspace(c)) {
                fputc('\n', out);
            } else {
                fputc(c, out);
            }
        }
    } else if (strcmp(d, "C") == 0) {
        while ((c = fgetc(in)) != EOF) {
            if (c == ',') {
                fputc('\n', out);
            } else {
                fputc(c, out);
            }
        }
    } else {
        num = atoi(d);
        counter = 0;

        while ((c = fgetc(in)) != EOF) {
            while (++counter % num != 0) { 
                fputc(c, out);
            }

            fputc('\n', out);

            counter = 0;
        }
    }

    fputs("^D", out);

    return 0;
}
