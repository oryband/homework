#include "Student.h"


using namespace std;


Student::Student(string sname, vector<string> *scourses) {
    name = string(sname);
    courses = new vector<string>(*scourses);
}


Student::~Student() {
    delete courses;
}
