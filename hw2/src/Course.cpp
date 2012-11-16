#include "../include/Course.h"

void Course :: teach(){

	std::vector<Student>::iterator student;

	// Activate in each student in this course the study() function.
	for (student = this->students.begin();
			student != this->students.end(); ++student) {

		student->study(this);

	}
}

unsigned short Course :: getMinGrade()const{

	return this->_minimumGrade;

}
