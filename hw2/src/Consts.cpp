#include "Consts.h"

#include <typeinfo>

using namespace std;

//not to forget to copy the while mess from uni.cpp in hw1 with the new and delete

vector< vector<string> >* Consts :: getLines(string filePath) {

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


void Consts :: readCoursesFile(string coursesPath, Department &cs,
												   Department &pg,
												   Department &el) { //to fix 1st

    vector< vector<string> >* lines = getLines(coursesPath);


    // Iterate over lines and copy data.
    size_t length = lines->size();
    for(unsigned int l=0; l < length; l++) {

        vector<string> line = (*lines)[l];  // Get line

        // Fetch words and cast to appropriate type.
        string _departmentName;
        string _courseName;
        unsigned short _activAtSemester;
        unsigned short _minGrade;
				//string id = line[1];
				//unsigned short space;

        istringstream oss1(line[0]);  // Cast department day.
        oss1 >> _departmentName;

        if( _departmentName.compare("CS") == 0 ) {

        	if ( line[2]%(2) == 1 ) {   //  Its autumn course

        		cs._autumnCourses.push_back(*new CsCourse(line[1],line[2],line[3]));
        	}
        	else {						//  Its spring course
        		cs._springCourses.push_back(*new CsCourse(line[1],line[2],line[3]));
        	}
        }
        if( _departmentName.compare("PG") == 0 ) {

			if ( line[2]%(2) == 1 ) {   //  Its autumn course

				pg._autumnCourses.push_back(*new PgCourse(line[1],line[2],line[3]));
			}
			else {						//  Its spring course
				pg._springCourses.push_back(*new PgCourse(line[1],line[2],line[3]));
			}
		}
        if( _departmentName.compare("ELECTIVE") == 0 ) {

			if ( line[2]%(2) == 1 ) {   //  Its autumn course

				pg._autumnCourses.push_back(*new ElCourse(line[1],line[2],line[3]));
			}
			else {						//  Its spring course
				pg._springCourses.push_back(*new ElCourse(line[1],line[2],line[3]));
			}
		}

    }
}


void Uni::readStudentsFile(string studentPath) { // to fix

    vector< vector<string> >* lines = getLines(studentPath);

    // Iterate over lines and copy data.
    size_t length = lines->size();
    for(unsigned int l=0; l < length; l++) {

        vector<string> line = (*lines)[l];  // Get line.
        vector<string>* appliedCourses = new vector<string>;  // New applied course list.
        vector<unsigned short>* weekdays = new vector<unsigned short>;

        string name = line[0];  // Student's name.

        size_t size = line.size();
        for(unsigned int word=1; word < size; word++){
            appliedCourses->push_back(line[word]);
        }

        this->unassignedStudents.push_back(
                *new Student(name, appliedCourses));
    }
}

void Consts :: readCurriculumFile(string studentPath) { // to fix

    vector< vector<string> >* lines = getLines(studentPath);

    // Iterate over lines and copy data.
    size_t length = lines->size();
    for(unsigned int l=0; l < length; l++) {

        vector<string> line = (*lines)[l];  // Get line.
        vector<string>* appliedCourses = new vector<string>;  // New applied course list.
        vector<unsigned short>* weekdays = new vector<unsigned short>;

        string name = line[0];  // Student's name.

        size_t size = line.size();
        for(unsigned int word=1; word < size; word++){
            appliedCourses->push_back(line[word]);
        }

        this->unassignedStudents.push_back(
                *new Student(name, appliedCourses));
    }
}

