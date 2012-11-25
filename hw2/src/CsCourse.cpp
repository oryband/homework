#include "CsCourse.h"

#include "Student.h"

#include <iostream>
using namespace std;

void CsCourse :: reg(Student &s) {
    this->pushToCourse(&s);

    writeToStudentsLogFile(
            s.getStudentId(),
            this->_name,
            CS,
            TAKING_COURSE);
}
