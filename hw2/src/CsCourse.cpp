#include "CsCourse.h"


void CsCourse :: reg(Student &student) {
    this->pushToCourse(&student);
    writeToFileStudents(student.getStudentId(), this->_name, CS, 1);
}


void CsCourse :: teach() {
	vector<Student *>::iterator it_student;

	for (it_student = this->_students.begin();
            it_student != this->_students.end(); ++it_student) {

        this->teachStudent(**it_student);
	}
}


// FIXME
void CsCourse :: teachStudent(Student &student) {
    Course *copy = this;
    student.study(*copy);
}
