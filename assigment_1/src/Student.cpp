#include "Student.h"


using namespace std;


Student::Student(string sname, vector<string>* scourses) {
    name = sname;
    courses = new vector<string>;

    for (int i=0; i < scourses->size(); i++) {
        courses->push_back((*scourses)[i]);
    }

    //*courses = *scourses;
}


Student::~Student() {
    delete courses;
}
