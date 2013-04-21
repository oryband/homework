#include <stdio.h>
#include <string.h>
#include <stdlib.h>


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


int main(int argc, char *argv[]) {
    link *list = NULL;

    list = list_append(list, 1);
    list = list_append(list, 2);
    list = list_append(list, 3);

    printf("The list size is: %d\n", list_size(list));
    printf("The list content is: [ ");
    list_print(list);
    printf("]\n");
    list_free(list);
}
