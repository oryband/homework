#include <iostream>
#include <math.h>

using namespace std;


long base2(int n) {
    int power=0,
        result=0;

    // Using descending powers of 2 and substraction method:
    while (n != 0) {
        // Fit the biggest power of 2 into n.
        while (pow(2, ++power) <= n);
        --power;

        // Add `1` into the correspnding bit that represents that power.
        result += pow(10, power);

        // Substract this power from the original amount.
        n -= pow(2, power);

        // Repeat until done.
        power = 0;
    }

    return result;
}


int main(int argc, char* argv[]) {
    int n;
    cin >> n;
    cout << base2(n) << endl;
}
