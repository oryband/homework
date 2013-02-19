#include "util.h"


using namespace std;


void getLines(string filePath, vector< vector<string> > *lines) {

    string line;
    ifstream file;

    file.open(filePath.c_str());

    if (!file) {
        cout << "Unable to open " << filePath <<  endl;
        exit(1);  // Terminate with error.
    }

    while(getline(file,line)) {
        vector<string> words;
        boost::split(words, line, boost::is_any_of(","));

        (*lines).push_back(words);
    }

    file.close();
}


void writeNumOfSemesterToFile(int semester) {

    ofstream randomFile;
    randomFile.open(RANDOM_FILE, fstream::in | fstream::app);

    string tail = " Semester Of The Random U.";

    switch (semester) {
        case 1:
            randomFile << endl << "1st" << tail << endl;
            break;
        case 2:
            randomFile << endl << "2nd" << tail << endl;
            break;
        case 3:
            randomFile << endl << "3rd" << tail << endl;
            break;
        default:
            randomFile << endl << semester << "th" << tail << endl;
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
        case QUITS_COURSE:
            randomFile << studentId << " quits course " << courseName << endl;
            break;
        case SLACKING_COURSE:
            randomFile << studentId << " is slacking course " << courseName << endl;
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

    randomFile.close();
}
