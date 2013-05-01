#include <stdio.h>
#include <stdlib.h>

/*#include "oop.c"*/

int inc(int i) { return i+1; }
int dec(int i) { return i-1; }
int iprt(int i) { printf("%d\n", i); return i; }

typedef struct int_array {
  int *array;
  int sz;
  void (*map) (struct int_array *, int (*f) (int));
} int_array;

void int_array_map(int_array *iarray, int (*f) (int)) {
    rec_map(iarray->array, iarray->sz, f);
}

void initialize_int_array(int_array *iarray, int *array, int sz) {
    iarray->array = array;
    iarray->sz = sz;
    iarray->map = int_array_map;
}

void rec_map(int *array, int sz, int (*f) (int)) {
    if (sz == 0) {
        return;
    } else {
        array[0] = f(array[0]);
        return rec_map(array+1, sz-1, f);
    }
}

typedef struct fun_desc {
    char *name;
    int (*fun) (int);
} fun_desc;


int main(int argc, char **argv) {
    int size,
        i,
        *a;
    int_array ia;
    fun_desc fs[3];


    /* -- 1 -- */
    printf("Array size [0, 10]: ");
    scanf("%d", &size);

    a = (int*) calloc (size, sizeof(int));

    for (i=0; i<size; i++) {
        printf("Array[%d]: ", i);
        scanf("%d", &a[i]);
    }

    initialize_int_array(&ia, a, size);

    /* -- 2 -- */
    fs[0].name = "inc";
    fs[1].name = "dec";
    fs[2].name = "iprt";
    fs[0].fun = inc;
    fs[1].fun = dec;
    fs[2].fun = iprt;

    /* -- 3 -- */
    i = -1;
    while (i>3 || i<1) {
        printf("\n1. %s\n2. %s\n3. %s\n\nFunction: ",
                fs[0].name, fs[1].name, fs[2].name);
        scanf("%d", &i);
    }

    ia.map(&ia, fs[i-1].fun);

    free(a);
    return 0;
}
