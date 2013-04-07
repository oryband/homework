#include <stdio.h>
#include <string.h>
#include <stdlib.h>


int addr5;  // BSS
int addr6;  // "

int foo();  // Init segment: Read-only.
void point_at(void *p);  // "


int main (int argc, char** argv) {
    static int addr1;  // BSS

    int addr2;  // Init segment: Read-Write.
    int addr3;

    char* yos="ree";  // Init segment: Read-only.

    int * addr4 = (int*)(malloc(50));  // Heap.

    int iarray[3];  // Init segment: Read-Write.
    char carray[3];  // "

    int i;  // Stack

    printf("%p\n",&addr1);
    printf("%p\n",&addr2);
    printf("%p\n",&addr3);
    printf("%p\n",foo);
    printf("%p\n",&addr5);
    point_at(&addr5);
    printf("%p\n",&addr6);
    printf("%p\n",yos);
    printf("%p\n",addr4);

    printf("--- iarray ---\n");

    for (i=0; i<sizeof(iarray)/sizeof(iarray[0]); i++) {
        printf("%p\n", &iarray[i]);
    }

    printf("--- carray ---\n");

    for (i=0; i<sizeof(carray)/sizeof(carray[0]); i++) {
        printf("%p\n", &carray[i]);
    }

    printf(" --- i/carray +0/+1---\n");
    printf("%p\n", &iarray);
    printf("%p\n", &iarray+1);
    printf("%p\n", &carray);
    printf("%p\n", &carray+1);

    return 0;
}


int foo() {
    return -1;
}


void point_at(void *p) {
    int local;
    long dist1 = (size_t)&addr6 - (size_t)p;
    long dist2 = (size_t)&local - (size_t)p;
    long dist3 = (size_t)&foo - (size_t)p;
    printf("dist1: %ld\n",dist1);
    printf("dist2: %ld\n",dist2);
    printf("dist3: %ld\n",dist3);
}
