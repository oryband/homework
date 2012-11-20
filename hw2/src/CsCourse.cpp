#include "CsCourse.h"


void CsCourse :: reg(Student &s) {
    this->pushToCourse(&s);

    writeToStudentsLogFile(
            s.getStudentId(),
            this->_name,
            CS,
            TAKING_COURSE);
}
