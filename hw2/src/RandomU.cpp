#include <string>
#include <vector>
#include <iostream>

#include <boost/algorithm/string.hpp>

#include "Uni.h"


using namespace std;


int main(int argc, char *argv[]) {
    vector<string> words;
    boost::split(words, argv[1], boost::is_any_of("="));  // FIXME Try '=' instead of "=".

    // FIXME use lowercase() of some sort.
    bool pgOn;
    if (words[1].compare("yes") == 0) {
        pgOn = true;
    } else {
        pgOn = false;
    }

    Uni uni(pgOn);
    uni.simulate();

    return 0;
}
