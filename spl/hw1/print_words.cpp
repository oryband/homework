#include <iostream>
#include <string>
#include <vector>

using namespace std;


vector<string> split(string sentence) {
    vector<string> words;

    for (unsigned int i=0, b=0; i <= sentence.size(); i++) {
        if (sentence[i] == ' ') {
            if (b != i) {  // Don't push multiple spaces.
                words.push_back(sentence.substr(b, i - b));
                b = i + 1;
            } else {
                b++;
            }
        }
    }

    return words;
}


void printWords(vector<string> words) {
    vector<string>::iterator w;
    for(w = words.begin(); w != words.end(); w++) {
        cout << *w << endl;
    }
}


int main(int argc, char* argv[]) {
    string sentence;
    getline(cin, sentence);
    printWords(split(sentence));
}
