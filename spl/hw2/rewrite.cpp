#include <stdio.h>
#include <iostream>
#include <string>

using namespace std;


int* SomeFunction() {
    int* pPointer = new int;  // Allocate memory.

    *pPointer = 25;  // Assign value.

    return pPointer;  // Return address.
}


int main(int argc, char* argv[]) {
    int* pPointer = SomeFunction();  // Make pPointer point to the int.

    cout << "Value of *pPointer: "<< *pPointer << endl;

    if (0 != pPointer) {  // Safe delete, since share it in two functions.
        delete pPointer;
        pPointer = 0;
    }
}
