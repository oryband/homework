#include <stdio.h>
#include <string.h>
#include <stdlib.h>


typedef struct link link;


struct link {
    link *next;
    int data;
};
