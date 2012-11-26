#include "CsCourse.h"

#include "Student.h"


using namespace std;


void CsCourse :: reg(Student &s) {
    this->pushToCourse(&s);

    writeToStudentsLogFile(
            s.getStudentId(),
            this->_name,
            _CS_,
            TAKING_COURSE);
}
