#include "CsCourse.h"


void CsCourse :: reg(Student &s) {
    this->pushToCourse(&s);

    Util::writeToStudentsLogFile(
            s.getStudentId(),
            this->_name,
            CS,
            TAKING_COURSE);
}

CsCourse :: ~CsCourse() { cout << "CsCourse dead" << endl; }
