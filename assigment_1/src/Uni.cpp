#include "Uni.h"


#include <typeinfo>

using namespace std;


Uni::Uni(string coursesPath, string studentsPath) {

    readCoursesFile(coursesPath);
    readStudentsFile(studentsPath);
}


/* Iterate over each student's applied courses,
 * and assign them to available courses.
 */
void Uni::assignStudents() {

    vector<Student>::iterator student;
    vector<string>::iterator cid;  // Course id.

    vector< vector<Course> >::iterator day;
    vector<Course>::iterator course;

    // Assign students to course.
    for (student = this->unassignedStudents.begin();
            student != this->unassignedStudents.end(); ++student) {

        cout << student->name << endl;

        // Assign each applied course to course in week.
        for (cid = student->courses.begin();
                cid != student->courses.end(); ++cid) {

			// Iterate in each day the current course
			for (course = this->courses.begin();
					course != this->courses.end(); ++course) {

				cout << *cid << endl;

				if (course->id.compare(*cid) && course->space > 0) {
					cout << "found!" << endl;

					course->assignedStudents.push_back(*student);
					course->space--;
				}
			}
        }
    }
}


vector< vector<string> >* Uni::getLines(string filePath) {

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

        unsigned int b = 0,  // Begin index.
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


void Uni::readCoursesFile(string coursesPath) {

    vector< vector<string> >* lines = getLines(coursesPath);


    // Iterate over lines and copy data.
    size_t length = lines->size();
    for(unsigned int l=0; l < length; l++) {

        vector<string> line = (*lines)[l];  // Get line

        // Fetch words and cast to appropriate type.
        unsigned short weekday;
        string id = line[1];
        unsigned short space;

        istringstream oss1(line[0]);  // Cast weekday.
        oss1 >> weekday;

        istringstream oss2(line[2]);  // Cast space.
        oss2 >> space;

        // Push course to appropriate weekday.
        this->courses.push_back(*new Course(weekday, id, space));
    }
}


void Uni::readStudentsFile(string studentPath) {

    vector< vector<string> >* lines = getLines(studentPath);

    // Iterate over lines and copy data.
    size_t length = lines->size();
    for(unsigned int l=0; l < length; l++) {

        vector<string> line = (*lines)[l];  // Get line.
        vector<string>* courses = new vector<string>;  // New applied course list.

        string name = line[0];
        size_t size = line.size();
        for(unsigned int word=1; word < size; word++){
            courses->push_back(line[word]);
        }

        this->unassignedStudents.push_back(*new Student(name, courses));
    }
}
