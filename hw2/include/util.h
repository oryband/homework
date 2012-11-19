#ifndef UTIL_H_
#define UTIL_H_

#include "consts.h"

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

void writeToFileNumOfSemester(int semester) {
    
    ofstream randomFile;
    randomFile.open(RANDOM_FILE, fstream::in | fstream::app);

    if (semester == 1) {
        
        randomFile << "1st Semester Of The Random U." << endl;
    }
    
    else if (semester == 2) {

        randomFile << "2st Semester Of The Random U." << endl;
    }

    else if (semester == 3) {

        randomFile << "3st Semester Of The Random U." << endl;
    }
    else {

        randomFile << semester << "th Semester Of The Random U." << endl;
    }
    randomFile.close();
}

// status options: 
// 1 - Student is taking a course.
// 2 - Student took and finish course successfully.
// 3 - Student took and finish course unsuccessfully.
// 4 - Student has graduated.
// 5 - Student has not graduated.
void writeToFileStudents(string studentId,
                         string courseName,
                         string department,
                         int status) {

    ofstream randomFile;
    randomFile.open(RANDOM_FILE, fstream::in | fstream::app);

    //  Student is taking course.
    if (status == 1) {
    
        randomFile << studentId << " is taking " << courseName 
                   << " from " << department << endl;
    }
    //  Student took and finish course successfully.
    if (status == 2) {
    
        randomFile << studentId << " took " << courseName 
                   << " and finished SUCCESSFULLY" << endl;
    }
    //  Student took and finish course unsuccessfully.
    if (status == 3) {
    
        randomFile << studentId << " took " << courseName 
                   << " and finished UNSUCCESSFULLY" << endl;
    }
    //  Student has graduated.
    if (status == 4) {
    
        randomFile << studentId << " has graduated" << endl;
    }
    //  Student has not graduated.
    if (status == 5) {
    
        randomFile << studentId << " has not graduated" << endl;
    }
}

#endif
