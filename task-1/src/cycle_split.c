#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>


FILE *nextFile(FILE *outs[], int i) {
    int len = sizeof(outs) / sizeof(outs[0]);

    if (i > len -1) {
        i = 0;
    }
    while (outs[i] == 0) {
        i++;
        if (i == len -1) {
            i = 0;
        }
    }

    return outs[i];
}

void W(FILE *in, FILE *outs[], const char *d) {
    char c, i=0;
    FILE *out = outs[0];
    while ((c = fgetc(in)) != EOF) {
        if (isspace(c)) {
            fputc('\n', out);
            out = nextFile(outs, ++i);
        } else {
            fputc(c, out);
        }
    }
}


/*void C(FILE *in, FILE *outs, const char *d) {
    char c;
    while ((c = fgetc(in)) != EOF) {
        if (c == ',') {
            fputc('\n', outs);
        } else {
            fputc(c, outs);
        }
    }
}*/


/*void D(FILE *in, FILE *outs, const char *d) {
    char c;
    int num = atoi(d),
        counter = 0;

    while (c != EOF) { 
        while ((c = fgetc(in)) != EOF && ++counter < num) {
            fputc(c, outs);
        }

        fputc('\n', outs);

        counter = 0;
    }
}*/


int main(int argc, char **argv) {
    FILE *outs[3]={ stdout, 0, 0 };
    FILE *out;
    FILE *in = stdin;

    char *d = "W";

    int i, j=0, len;


    puts("---");

    for (i=1; i < argc; i++) {
        if (strcmp(argv[i],"-i") == 0) {
            in = fopen(argv[++i], "r");
        } else if (strcmp(argv[i],"-d") == 0) {
            d = argv[++i];
        } else {
            if (j > 3) {
                printf("Output files should be <= 3");
                return -1;
            }

            if (strcmp("stdout", argv[i]) != 0) {
                outs[j++] = fopen(argv[i], "w");
            } else {
                outs[j++] = stdout;
            }
        }
    }

    if (strcmp(d, "W") == 0) {
        W(in, outs, d);
    } else if (strcmp(d, "C") == 0) {
        0;
        /*C(in, outs, d);*/
    } else {
        0;
        /*D(in, outs, d);*/
    }

    /*fputs("^D", outs);*/

    len = sizeof(outs) / sizeof(outs[0]);
    for (i=0; i < len; i++) {
        if (out != stdout && out != 0) {
            fclose(out);
        }
    }

    if (in != stdin) {
        fclose(in);
    }

    return 0;
}
