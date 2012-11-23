#ifndef UTIL_H_
#define UTIL_H_


#include <iostream>
#include <fstream>
#include <string>
#include <vector>

#include <cstdlib>
#include "consts.h"


using namespace std;

namespace Util
{
inline void getLines(string filePath, vector< vector<string> >* lines) {

    string line;
    ifstream file;

    file.open(filePath.c_str());

    if (!file) {
        cout << "Unable to open " << filePath <<  endl;
        exit(1);  // Terminate with error.
    }
    //TODO print_tests
    cout << " in get lines before npos " << endl;
    while (file >> line) {
        vector<string> words;

        unsigned int b = 0;  // Begin index.
        int e = line.find(',');  // End index.
        while (e != string::npos) {  // FIXME - DONT CAST IT CAUSES BUGS.
            words.push_back(line.substr(b, e - b));

            b = e+1;
            e = line.find(',', b);
        }

     //TODO print_tests
     cout << " in get lines after npos " << endl;
        words.push_back(line.substr(b, line.size() - b));

       (*lines).push_back(words);
    }
    //TODO print_tests
    cout << " about to close file" << endl;
    file.close();
    cout << "file is closed" << endl;
}


inline void writeNumOfSemesterToFile(int semester) {
    
    ofstream randomFile;
    randomFile.open(RANDOM_FILE, fstream::in | fstream::app);

    string tail = " Semester Of The Random U.";

    switch (semester) {
        case 1:
            randomFile << "1st" << tail << endl;
            break;
        case 2:
            randomFile << "2nd" << tail << endl;
            break;
        case 3:
            randomFile << "3rd" << tail << endl;
            break;
        default:
            randomFile << semester << "th" << tail << endl;
            break;
    }

    randomFile.close();
}


inline void writeToStudentsLogFile(
        string studentId,
        string courseName,
        string department,
        int status) {

    ofstream randomFile;
    randomFile.open(RANDOM_FILE, fstream::in | fstream::app);

    switch (status) {
        case TAKING_COURSE:
            randomFile << studentId << " is taking " << courseName <<
                " from " << department << endl;
            break;
        case FINISHED_COURSE:
            randomFile << studentId << " took " << courseName <<
                " and finished SUCCESSFULLY" << endl;
            break;
        case FAILED_COURSE:
            randomFile << studentId << " took " << courseName <<
                " and finished UNSUCCESSFULLY" << endl;
            break;
        case GRADUATED:
            randomFile << studentId << " has graduated" << endl;
            break;
        case NOT_GRADUATED:
            randomFile << studentId << " has not graduated" << endl;
            break;
        case DENIED:
            randomFile << studentId <<
                " is being denied his education" << endl;
            break;
    }
}
}
#endif
