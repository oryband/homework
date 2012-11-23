#include "util.h"


using namespace std;


void getLines(string filePath, vector< vector<string> > &lines) {

    string line;
    ifstream file;

    file.open(filePath.c_str());

    if (!file) {
        cout << "Unable to open " << filePath <<  endl;
        exit(0);  // Terminate with error.
    }

    while (file >> line) {
        vector<string> words;

        unsigned int b = 0;  // Begin index.
        size_t e = line.find(',');  // End index.
        while (e != string::npos) {  // FIXME - If bugs occur use `int e` instead of `size_t e`.
            words.push_back(line.substr(b, e - b));

            b = e+1;
            e = line.find(',', b);
        }

        words.push_back(line.substr(b, line.size() - b));

        lines.push_back(words);
    }

    file.close();
}


void writeNumOfSemesterToFile(int semester) {
    
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


void writeToStudentsLogFile(
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
