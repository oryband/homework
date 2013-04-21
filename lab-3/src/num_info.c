#include <stdio.h>
#include <string.h>
#include <stdlib.h>


/* Task 1 */
typedef struct link link;

struct link {
    link *next;
    int data;
};


/* Task 1.a */

/* Print the data of every link in list.
 * Each data is followed by a newline character. */
void list_print(link *list) {
    printf("%d ", list->data);

    if (list->next == NULL) {
        return;
    } else {
        return list_print(list->next);
    }
}


/* Return the number of elements in the list. */
int list_size(link *list) {
    if (list->next == NULL) {
        return 1;
    } else {
        return 1 + list_size(list->next);
    }
}


/* Add a new link with the given data to the end
 * of the list and return a pointer to the list (i.e., the first link in the list).
 * If list is null - create a new link and return a pointer to the link. */
link *list_append(link *list, int data) {
    if (list == NULL) {
        list = (link*) malloc (sizeof(link*));
        list->next = NULL;
        list->data = data;
        return list;
    } else {
        list->next = list_append(list->next, data);
        return list;
    }
}


/* Free the memory allocated by the list. */
void list_free(link *list) {
    if (list->next != NULL) {
        list_free(list->next);
    }

    free(list);
    return;
}

/* Task 1.b */
int reduce(link *l, int (*f)(int, int), int iv) {
    if (l == NULL) {
        return iv;
    } else {
        return reduce(l->next, f, f(iv, l->data));
    }
}


/* Returns the sum of a and b */
int sum(int a, int b) {
    return a+b;
}


/* Returns the maximum of a and b */
int max(int a, int b) {
    return a>b ? a : b;
}


/* Task 1.c */
link *map(link *l, int (*f)(int)) {
    if (l == NULL) {
        return NULL;
    } else {
        link *n = (link*) malloc (sizeof(link*));
        n->data = f(l->data);
        n->next = map(l->next, f);
        return n;
    }
}

/* returns a if a is an odd number, 0 otherwise */
int odd(int a) {
    return a%2 == 1 ? a : 0;
}

/* returns a if a is greater than 5, 0 otherwise */
int gt5(int a) {
    return a>5 ? a : 0;
}
/* returns the square of a */
int square(int a) {
    return a*a;
}


int main(int argc, char *argv[]) {
    /* Task 1.a */
    /*link *list = NULL;
    list = list_append(list, 1);
    list = list_append(list, 2);
    list = list_append(list, 3);
    printf("The list size is: %d\n", list_size(list));
    printf("The list content is: [ ");
    list_print(list);
    printf("]\n");
    list_free(list);*/

    /* Task 1.b */
    /*link *list = NULL;
    list = list_append(list, 1);
    list = list_append(list, 2);
    list = list_append(list, 3);
    printf("The sum of all list values: %d\n", reduce(list,sum,0));
    printf("The max value of list links: %d\n", reduce(list,max,0));
    printf("The list content is: [ ");
    list_print(list);
    printf("]\n");
    list_free(list);*/

    /* Task 1.c */
    /*link *list = NULL;
    link *new_list = NULL;
    int res = 0;

    list = list_append(list, 1);
    list = list_append(list, 3);
    list = list_append(list, 8);

    new_list = map(list,odd);
    res = reduce(new_list, max, 0);

    printf("The maximal odd number in the list is: %d\n", res);
    printf("new_list content is: [ ");
    list_print(new_list);
    printf("]\n");

    list_free(list);
    list_free(new_list);*/
}
