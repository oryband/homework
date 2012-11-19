#include "PgCourse.h"


void PgCourse :: reg(Student &student){
    this->pushToCourse(&student);
    writeToFileStudents(student.getStudentId(), this->_name, PG, 1);
}
