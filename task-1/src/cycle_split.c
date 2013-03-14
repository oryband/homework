#include <stdio.h>
#include <string.h>


int main(int argc, char **argv) {
    FILE *out=stdout,
         *in=stdin;
    char c;

    puts("---");

    while ((c = fgetc(in)) != EOF) {
        if (isspace(c)) {
            fputc('\n', out);
        } else {
            fputc(c, out);
        }
    }

    fputs("^D", out);

    return 0;
}
