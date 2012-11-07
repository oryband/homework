#include "Course.h"


using namespace std;


Course::Course(string cid, unsigned short cspace) {
    id = cid;
    space = cspace;
    assignedStudents = new vector<Student>;
}


Course::~Course() {
    delete assignedStudents;
}
