#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>


void W(FILE *in, FILE *out, const char *d) {
    char c;
    while ((c = fgetc(in)) != EOF) {
        if (isspace(c)) {
            fputc('\n', out);
        } else {
            fputc(c, out);
        }
    }
}


void C(FILE *in, FILE *out, const char *d) {
    char c;
    while ((c = fgetc(in)) != EOF) {
        if (c == ',') {
            fputc('\n', out);
        } else {
            fputc(c, out);
        }
    }
}


void D(FILE *in, FILE *out, const char *d) {
    char c;
    int num = atoi(d),
        counter = 0;

    while (c != EOF) { 
        while ((c = fgetc(in)) != EOF && ++counter < num) {
            fputc(c, out);
        }

        fputc('\n', out);

        counter = 0;
    }
}


int main(int argc, char **argv) {
    FILE *out=stdout,
         *in=stdin;
    char *d = "W";
    int i;

    puts("---");

    for (i=1; i < argc; i++) {
        if (strcmp(argv[i],"-i") == 0) {
            in = fopen(argv[++i], "r");
        } else if (strcmp(argv[i],"-d") == 0) {
            d = argv[++i];
        } else {
            printf("invalid parameter - %s\n", argv[i]);
            return 1;
        }
    }

    if (strcmp(d, "W") == 0) {
        W(in, out, d);
    } else if (strcmp(d, "C") == 0) {
        C(in, out, d);
    } else {
        D(in, out, d);
    }

    /*fputs("^D", out);*/

    if (out != stdout) {
        fclose(out);
    }

    if (in != stdin) {
        fclose(in);
    }

    return 0;
}
