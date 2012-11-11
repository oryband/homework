#include "Uni.h"


#include <typeinfo>

using namespace std;


Uni::Uni(string coursesPath, string studentsPath):
    courses(), unassignedStudents() {
    
    vector< vector<string> >* coursesLines = new vector< vector<string> >;

    getLines(coursesPath, coursesLines);
    readCourses(coursesLines);

    vector< vector<string> >* studentsLines = new vector< vector<string> >;

    getLines(studentsPath, studentsLines);
    readStudents(studentsLines);

    delete coursesLines;
    delete studentsLines;
}


void Uni::getLines(string filePath,vector< vector<string> >* lines) {

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
        while (e != string::npos) {  // WARNING: Setting e to unsigned causes a wierd index bug.
            words.push_back(line.substr(b, e - b));

            b = e+1;
            e = line.find(',', b);
        }

        words.push_back(line.substr(b, line.size() - b));

        lines->push_back(words);
    }

    file.close();
}


void Uni::readCourses(vector< vector<string> >* lines) {

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
        this->courses.push_back(Course(weekday, id, space));
    }
}


void Uni::readStudents(vector< vector<string> >* lines) {

    // Iterate over lines and copy data.
    size_t length = lines->size();
    for(unsigned int l=0; l < length; l++) {
        vector<string> appliedCourses;  // New applied course list.

        vector<string> line = (*lines)[l];  // Get line.

        string name = line[0];  // Student's name.

        size_t size = line.size();
        for(unsigned int word=1; word < size; word++){
            appliedCourses.push_back(line[word]);
        }

        this->unassignedStudents.push_back(Student(name, &appliedCourses));
    }
}


/* Iterate over each student's applied courses,
 * and assign them to available courses.
 */
void Uni::assignStudents() {

    vector<Student>::iterator student;
    vector<string>::iterator cid;  // Course id.
    int cidIndex;

    vector< vector<Course> >::iterator day;
    vector<Course>::iterator course;

    bool found;

    // Assign students to course.
    for (student = this->unassignedStudents.begin();
            student != this->unassignedStudents.end(); ++student) {

        // Assign each applied course to course in week.
        for (cid = student->courses.begin(), cidIndex = 0;
                cid != student->courses.end(); ++cid, cidIndex++) {
            
            found = false;

			// Iterate in each day the current course
			for (course = this->courses.begin();
					!found && course != this->courses.end(); ++course) {

                // TODO: Replace compare and '!'
				if ( ! course->id.compare(*cid) && course->space > 0) {
                    found = true;

                    student->weekdays[cidIndex] = course->weekday;
                    course->assignedStudents.push_back(*student);
                    course->space--;
				}
			}
        }
    }
}


void Uni::printAssignment(
        string coursesOutputPath, string studentsOutputPath) {

    printCoursesToFile(coursesOutputPath);
    printStudentsToFile(studentsOutputPath);
}


void Uni::printCoursesToFile(string coursesOutputPath) {

    ofstream coursesFile;
    coursesFile.open(coursesOutputPath.c_str());
    
    vector<Student>::iterator student;
    vector<Course>::iterator uniCourse;

    for (int day = 1 ; day <= 7 ; day++) {
        for (uniCourse = this->courses.begin(); 
                uniCourse != this->courses.end(); ++uniCourse) {

            if (uniCourse->weekday == day) {

                coursesFile << uniCourse->weekday << "\t" << uniCourse->id << endl;

                for (student = uniCourse->assignedStudents.begin();
                        student != uniCourse->assignedStudents.end(); 
                        ++student) {

                    coursesFile << "\t" << student->name << endl;
                }

                coursesFile << endl;
            }
        }
    }

    coursesFile.close();
}


void Uni::printStudentsToFile(string studentsOutputPath) {

    ofstream studentsFile;
    studentsFile.open(studentsOutputPath.c_str());

    vector<Student>::iterator student;
    vector<string>::iterator appliedCourse;
    int cIndex;  // Course index.

    for (student = this->unassignedStudents.begin();
            student != this->unassignedStudents.end(); ++student) {

        studentsFile << student->name << endl;

        for (int day = 1 ; day <= 7 ; day++) {

            for (appliedCourse = student->courses.begin(), cIndex = 0;
                    appliedCourse != student->courses.end();
                    ++appliedCourse, cIndex++) {

                if (student->weekdays[cIndex] == day) {
                    studentsFile << "\t" <<
                        student->weekdays[cIndex] << " " <<
                        *appliedCourse << endl;
                }

            }

        }

        studentsFile << endl;
    }

    studentsFile.close();
}
