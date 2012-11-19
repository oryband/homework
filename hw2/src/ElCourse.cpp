#include "ElCourse.h"


void ElCourse :: reg(Student &student) {
    this->pushToCourse(&student);
    writeToFileStudents(student.getStudentId(), this->_name, ELECTIVE, 1);
}
