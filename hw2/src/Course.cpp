#include "Course.h"


using namespace std;


Course :: Course(
        std::string name,
        unsigned short semester,
        unsigned short minimumGrade) :

    _name(name),
    _semester(semester),
    _minimumGrade(minimumGrade),
    _students() {}


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
