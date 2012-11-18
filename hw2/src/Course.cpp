#include "Course.h"

void Course :: teach() {
	std::vector<StudentPointer>::iterator it_student;

	for (it_student = this->_students.begin();
            it_student != this->_students.end(); ++it_student) {

		(**it_student).study(*this);
	}
}

unsigned short Course :: getMinimumGrade()const{
    return this->_minimumGrade;
}

std::vector<StudentPointer>* Course :: getStudents() {
    return &(this->_students);
}
