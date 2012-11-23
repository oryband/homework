#include "PgCourse.h"


void PgCourse :: reg(Student &s){
    this->pushToCourse(&s);

    Util::writeToStudentsLogFile(
            s.getStudentId(),
            this->_name,
            PG,
            TAKING_COURSE);
}

PgCourse :: ~PgCourse() { cout << "PgCourse dead" << endl; }
