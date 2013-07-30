#include <stdio.h>

int main(int argc, char *argv[]) {
    int x=3;
    int *y = &x;

    printf("&x: %p\n", &x);
    printf("y: %p\n", y);

    printf("x: %d\n", x);
    printf("*y: %d\n", *y);
}
