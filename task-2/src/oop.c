#include <stdio.h>

/* forward declarations: */
void rec_map(int*, int, int (*f) (int));


struct int_array {
  int *array;
  int sz;
  void (*map) (struct int_array *, int (*f) (int));
};


void int_array_map(struct int_array *iarray, int (*f) (int)){
  /* TODO: complete during task 2.c */
}


void initialize_int_array(struct int_array *iarray, int *array, int sz){
  /* TODO: complete during task 2.c */
}


/* TODO: 2.a functions here */


void rec_map(int *array, int sz, int (*f) (int)){
  /* TODO: Complete during task 2.b */
}


int main(int argc, char **argv){
  /* TODO: Test your code */
}
