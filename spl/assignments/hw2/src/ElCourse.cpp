#include "ElCourse.h"

#include "Student.h"


void ElCourse :: reg(Student &s) {
    this->pushToCourse(&s);

    writeToStudentsLogFile(
            s.getId(),
            this->_name,
            _ELECTIVE_,
            TAKING_COURSE);
}
