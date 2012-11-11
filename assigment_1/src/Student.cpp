#include "Student.h"


using namespace std;


Student::Student(string sname, vector<string>* scourses):
    name(sname), courses(), weekdays() {

    for (unsigned int i=0; i < scourses->size(); i++) {
        courses.push_back((*scourses)[i]);
        weekdays.push_back(0);
    }
}
