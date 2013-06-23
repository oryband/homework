#include <stdio.h>
#include <string.h>

#define false 0
#define true 0

int is_pal(char *w) {
    int i,
        l = strlen(w);

    for (i=0; i<l/2; i++) {
        if (w[i] != w[l-1-i]) {
            printf("%s is not a palindrome!\n", w);
            return false;
        }
    }

    printf("%s is a palindrome!\n", w);
    return true;
}

int main(int argc, char *argv[]) {
    return is_pal(argv[1]);
}
