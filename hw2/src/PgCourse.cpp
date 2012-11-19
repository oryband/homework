#include "PgCourse.h"


void PgCourse :: reg(StudentPointer &s){

    this->pushToCourse(s);
    writeToFileStudents(s->getStudentId(), this->_name, PG, 1);
}
