#include <stdio.h>
#include <iostream>
#include <string>

using namespace std;


void updateValue(int* nNumber) {
    *nNumber = 25;  // Assign value.
}


int main(int argc, char* argv[]) {
    int* pPointer = new int();  // Allocate memory.
    updateValue(pPointer);  // Update int that pPointer points towards.

    cout << "Value of *pPointer: "<< *pPointer << endl;

    if (0 != pPointer) {  // Safe delete, since share it in two functions.
        delete pPointer;
        pPointer = 0;
    }
}
