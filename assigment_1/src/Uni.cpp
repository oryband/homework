#include "Uni.h"


using namespace std;


Uni::Uni(string coursesPath, string studentsPath) {

    weekdays = new vector< vector<Course>* >;

    // Create seven courses-list for each day.
    for (int d=0; d < DAYS; d++) {
        weekdays->push_back(new vector<Course>);
    }
    
    readCoursesFile(coursesPath);
    readStudentsFile(studentsPath);
}


Uni::~Uni() {
    vector<Student>::iterator student;

    for (int d=0; d < DAYS; d++) {
        delete (*weekdays)[d];
    }

    delete weekdays;

    for (student = unassignedStudents.begin();
            student != unassignedStudents.end(); ++student) {
        delete &*student;
    }

    delete &unassignedStudents;
}


/* Iterate over each student's applied courses,
 * and assign them to available courses.
 */
void Uni::assignStudents() {
    
    vector<Student>::iterator student;
    vector<string>::iterator cid;  // Course id.

    vector< vector<Course>* >::iterator day;
    vector<Course>::iterator course;

    // Assign students to course.
    for (student = this->unassignedStudents.begin();
            student != this->unassignedStudents.end(); ++student) {

        // Assign each applied course to course in week.
        for (cid = student->courses->begin();
                cid != student->courses->end(); ++cid) {

            // Search for available course in week.
            for (day = this->weekdays->begin(); 
                    day != this->weekdays->end(); ++day) {

                // Iterate in each day the current course
                for (course = (*day)->begin();
                        course != (*day)->end(); ++course) {
                  
                    if (course->id.compare(*cid) && course->space > 0) {
                        course->assignedStudents->push_back(*student);
                        course->space--;
                    }
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
        cout << "Unable to open " << filePath << "file";
        exit(1);  // Terminate with error.
    }

    
    vector< vector<string> >* lines = new vector< vector<string> >;

    while (file >> line) {
        
        int size = line.length();
        vector<string>* words = new vector<string>;

        for (int c=0; c <= size;) {

            string word = "";
            while(line[c] != ',' && c <= size) {
                word += line[c];
                c++;
            }

            (*words).push_back(word);
        }

        (*lines).push_back(*words);
    }
    
    return lines;
}


void Uni::readCoursesFile(string coursesPath) {

    vector< vector<string> >* lines = getLines(coursesPath);

    // Iterate over lines and copy data.
    size_t length = (*lines).size();
    for(int l=0; l < length; l++) {

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
        vector <vector<Course>* > weekdays = *(this->weekdays);
        weekdays[weekday -1]->push_back(*new Course(id, space));
    }
}


void Uni::readStudentsFile(string coursesPath) {

    vector< vector<string> >* lines = getLines(coursesPath);

    // Iterate over lines and copy data.
    size_t length = (*lines).size();
    for(int l=0; l < length; l++) {

        vector<string> line = (*lines)[l];  // Get line
        vector<string>* stuCourses = new vector<string>;
        
        string name = line[0]; 
        size_t lineSize = line.size();

            for(int i=1; i < lineSize ;i++){
                    (*stuCourses).push_back(line[i]);
            }
        unassignedStudents.push_back(*new Student(name,stuCourses));
    }
}
