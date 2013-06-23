/* Cannot use includes, external functions. */
#include <stdio.h>  /* Not used in is_pal(). */

#define NULL 0
#define false 0
#define true 1
#define EXIT_SUCCESS 0

/* Cannot exceet ~300 bytes. */
int is_pal(char *w) {
    int i, l;

    for (l=0; w[l] != NULL; l++);  /* Calculate length. */

    for (i=0; i<l/2; i++) {
        if (w[i] != w[l-1-i]) {
            return false;
        }
    }

    return true;
}

int main(int argc, char *argv[]) {
    printf("%s is %sa palindrome!\n", argv[1], is_pal(argv[1]) ? "" : "not ");
    return EXIT_SUCCESS;
}

