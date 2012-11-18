#include "LinkedList.h"


int main(int argc, char *argv[]) {
    List list = List(),  // Link constructor.
         list2,
         list3;

    list2 = List(list);  // List copy-constructor.
    list3 = list2;  // Operator =

    list.insertData("first_link");  // Link constructor.

    const Link *link = list.getHead();
    Link *link2 = new Link(*link);  // Link copy-constructor.

    delete link2;  // link destructor.

    return 0;
}

// List destructor, more link destruction.
