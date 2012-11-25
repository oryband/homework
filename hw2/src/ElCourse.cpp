#include "ElCourse.h"

#include "Student.h"


void ElCourse :: reg(Student &s) {
    this->pushToCourse(&s);

    writeToStudentsLogFile(
            s.getStudentId(),
            this->_name,
            ELECTIVE,
            TAKING_COURSE);
}
