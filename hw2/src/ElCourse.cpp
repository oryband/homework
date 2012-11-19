#include "ElCourse.h"


void ElCourse :: reg(StudentPointer &s) {

    this->pushToCourse(s);
    writeToFileStudents(s->getStudentId(), this->_name, ELECTIVE, 1);
}
