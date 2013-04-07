#include <stdio.h>
#include <string.h>
#include <stdlib.h>


int addr5;
int addr6;

int foo();
void point_at(void *p);


int main (int argc, char** argv) {
    static int addr1;
    int addr2;
    int addr3;
    char* yos="ree";
    int * addr4 = (int*)(malloc(50));
    printf("%p\n",&addr1);
    printf("%p\n",&addr2);
    printf("%p\n",&addr3);
    printf("%p\n",foo);
    printf("%p\n",&addr5);
    point_at(&addr5);
    printf("%p\n",&addr6);
    printf("%p\n",yos);
    printf("%p\n",addr4);
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
