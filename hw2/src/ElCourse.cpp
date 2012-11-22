#include "ElCourse.h"

#include "Student.h"


ElCourse :: ElCourse(
        std::string name,
        unsigned short semester,
        unsigned short minimumGrade) :
    Course(name, ELECTIVE, semester, minimumGrade) {}


ElCourse :: ~ElCourse() {}


void ElCourse :: reg(Student &s) {
    this->pushToCourse(&s);

    writeToStudentsLogFile(
            s.getStudentId(),
            this->_name,
            ELECTIVE,
            TAKING_COURSE);
}
