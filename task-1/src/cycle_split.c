#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>


int main(int argc, char **argv) {
    FILE *out=stdout,
         *in=stdin;
    char c=0;
    char *d = "W";
    int i, num, counter;

    puts("---");

    for (i=1; i < argc; i++) {
        if (strcmp(argv[i],"-i") == 0) {
            out = fopen(argv[++i], "w");
        } else if (strcmp(argv[i],"-d") == 0) {
            d = argv[++i];
        } else {
            printf("invalid parameter - %s\n", argv[i]);
            return 1;
        }
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

        while (c != EOF) { 
            while ((c = fgetc(in)) != EOF && ++counter < num) {
                fputc(c, out);
            }

            fputc('\n', out);

            counter = 0;
        }
    }

    fputs("^D", out);

    if (out != stdout) {
        fclose(out);
    }

    return 0;
}
