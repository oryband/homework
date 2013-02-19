/** SPL Assignment #2
 * http://www.cs.bgu.ac.il/~spl131/Assignments/Assignment_2
 *
 * Eldar Damari damariel@post.bgu.ac.il
 * Ory Band oryb@post.bgu.acil
 */

#include <string>
#include <iostream>
#include <algorithm>

#include "Uni.h"


using namespace std;


int main(int argc, char *argv[]) {

    bool pgOn,
         error = false;

    string arg = "";

    // Error tests.
    if (argc != 2) {
        error = true;
    } else {
        // Get malag= argument and convert to lower case.
        arg = string(argv[1]);
        transform(arg.begin(), arg.end(), arg.begin(), ::tolower);
        if (arg.compare("malag=yes") != 0 && arg.compare("malag=no") != 0) {
            error = true;
        }
    }

    if (error) {
        cout << "Bad program arguments. Use 'malag=yes' or 'malag=no' only." << endl;
        exit(1);
    }

    if (arg == "malag=yes") {
        pgOn = true;
    } else {
        pgOn = false;
    }

    Uni uni(pgOn);
    uni.simulate();

    return 0;
}
