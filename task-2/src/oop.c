#include <stdio.h>
#include <stdlib.h>

/* forward declarations: */
void rec_map(int*, int, int (*f) (int));


/* --- 2a --- */
int inc(int i) {
    return i+1;
}
int dec(int i) {
    return i-1;
}
int iprt(int i) {
    printf("%d\n", i);
    return i;
}


/* --- 2b --- */
void rec_map(int *array, int sz, int (*f) (int)) {
    if (sz == 0) {
        return;
    } else {
        array[0] = f(array[0]);
        return rec_map(array+1, sz-1, f);
    }
}


/* -- 2c -- */
struct int_array {
  int *array;
  int sz;
  void (*map) (struct int_array *, int (*f) (int));
};


void int_array_map(struct int_array *iarray, int (*f) (int)) {
    rec_map(iarray->array, iarray->sz, f);
}


void initialize_int_array(struct int_array *iarray, int *array, int sz) {
    int i=0;

    iarray->array = array;
    iarray->sz = sz;
    iarray->map = int_array_map;
}


int main(int argc, char **argv){
    printf("-- 2a, 2b --\n");
    int a[] = {1,1,1};
    rec_map(a,3,iprt);
    rec_map(a,3,inc);
    rec_map(a,3,iprt);

    printf("-- 2c --\n");
    int array[3] = {1,1,1};
    struct int_array iarray;
    initialize_int_array(&iarray, array, 3);
    iarray.map(&iarray, iprt);
    iarray.map(&iarray, inc);
    iarray.map(&iarray, iprt);

    printf("__Clean exit.__\n");
}
