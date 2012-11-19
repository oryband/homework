#include "Course.h"


using namespace std;


Course :: Course(
        std::string name,
        unsigned short semester,
        unsigned short minimumGrade) :  // TODO Init department.

    _name(name),
    _semester(semester),
    _minimumGrade(minimumGrade),
    _students() {}


void Course :: teach() {
	vector<StudentPointer>::iterator it_student;

	for (it_student = this->_students.begin();
            it_student != this->_students.end(); ++it_student) {

		(*it_student)->study(*this);
	}
}

unsigned short Course :: getMinimumGrade()const{
    return this->_minimumGrade;
}

vector<StudentPointer>* Course :: getStudents() {
    return &(this->_students);
}


void  Course :: pushToCourse(StudentPointer &student) {

    this->_students.push_back(student);
    
    if (this->getCourseDepartment() == CS ||
        this->getCourseDepartment() == PG) {
        student->increaseUnfinishedSemesterCourses();
    }
}

string Course :: getCourseName()const {
    return this->_name;
}

string Course :: getCourseDepartment()const {
    return this->_departmentName;
}
