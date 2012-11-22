#include "PgCourse.h"

#include "Student.h"


void PgCourse :: reg(Student &s){
    this->pushToCourse(&s);

    writeToStudentsLogFile(
            s.getStudentId(),
            this->_name,
            PG,
            TAKING_COURSE);
}
