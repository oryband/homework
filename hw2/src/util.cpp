#include <iostream>
#include <fstream>
#include <sstream>
#include <iomanip>
#include <cstdlib>
#include <string>
#include <vector>


using namespace std;


vector< vector<string> >* getLines(string filePath) {

    string line;
    ifstream file;

    file.open(filePath.c_str());

    if (!file) {
        cout << "Unable to open " << filePath <<  endl;
        exit(1);  // Terminate with error.
    }


    vector< vector<string> >* lines = new vector< vector<string> >;

    while (file >> line) {
        vector<string> words;

        int b = 0,  // Begin index.
            e = line.find(',');  // End index.
        while (e != string::npos) {
            words.push_back(line.substr(b, e - b));

            b = e+1;
            e = line.find(',', b);
        }

        words.push_back(line.substr(b, line.size() - b));

        lines->push_back(words);
    }

    file.close();
    return lines;
}
