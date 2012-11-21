#ifndef UTIL_H_
#define UTIL_H_


#include <iostream>
#include <fstream>
#include <sstream>
#include <iomanip>
#include <cstdlib>
#include <string>
#include <vector>

#include "consts.h"
#include "util.h"

using namespace std;


void getLines(string filePath, vector< vector<string> > &lines) {

    string line;
    ifstream file;

    file.open(filePath.c_str());

    if (!file) {
        cout << "Unable to open " << filePath <<  endl;
        exit(1);  // Terminate with error.
    }

    while (file >> line) {
        vector<string> words;

        int b = 0,  // Begin index.
            e = line.find(',');  // End index.
        while (e != string::npos) {  // FIXME - DONT CAST IT CAUSES BUGS.
            words.push_back(line.substr(b, e - b));

            b = e+1;
            e = line.find(',', b);
        }

        words.push_back(line.substr(b, line.size() - b));

        lines.push_back(words);
    }

    file.close();
}


void writeToFileNumOfSemester(int semester) {
    
    ofstream randomFile;
    randomFile.open(RANDOM_FILE, fstream::in | fstream::app);

    if (semester == 1) {
        randomFile << "1st Semester Of The Random U." << endl;
    } else if (semester == 2) {
        randomFile << "2nd Semester Of The Random U." << endl;
    } else if (semester == 3) {
        randomFile << "3rd Semester Of The Random U." << endl;
    } else {
        randomFile << semester << "th Semester Of The Random U." << endl;
    }

    randomFile.close();
}


void writeToStudentsLogFile(
        string studentId,
        string courseName,
        string department,
        int status) {

    ofstream randomFile;
    randomFile.open(RANDOM_FILE, fstream::in | fstream::app);

    // TODO Use ENUM instead of status integers.
    //  Student is taking course.
    if (status == TAKING_COURSE) {
    
        randomFile << studentId << " is taking " << courseName 
                   << " from " << department << endl;
    }
    //  Student took and finish course successfully.
    if (status == FINISHED_COURSE) {
    
        randomFile << studentId << " took " << courseName 
                   << " and finished SUCCESSFULLY" << endl;
    }
    //  Student took and finish course unsuccessfully.
    if (status == FAILED_COURSE) {
    
        randomFile << studentId << " took " << courseName 
                   << " and finished UNSUCCESSFULLY" << endl;
    }
    //  Student has graduated.
    if (status == GRADUATED) {
    
        randomFile << studentId << " has graduated" << endl;
    }
    //  Student has not graduated.
    if (status == NOT_GRADUATED) {
    
        randomFile << studentId << " has not graduated" << endl;
    }
    if (status == DENIED) {
        
        randomFile << studentId << " is being denied his education" << endl;
    }
}

#endif
