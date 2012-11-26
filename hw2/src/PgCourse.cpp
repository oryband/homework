#include "PgCourse.h"

#include "Student.h"


void PgCourse :: reg(Student &s) {
    this->pushToCourse(&s);

    writeToStudentsLogFile(
            s.getStudentId(),
            this->_name,
            _PG_,
            TAKING_COURSE);
}
