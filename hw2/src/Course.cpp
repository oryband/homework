#include "Course.h"

#include "Student.h"


using namespace std;


Course :: Course(
        string name,
        string department,
        unsigned short semester,
        unsigned short minimumGrade) :
    _name(name),
    _department(department),
    _semester(semester),
    _minimumGrade(minimumGrade),
    _students() {}


void Course :: teach() {
	vector<Student *>::iterator it_student;

	for (it_student = this->_students.begin();
            it_student != this->_students.end(); ++it_student) {

        (**it_student).study(*this);
	}
}


const unsigned short Course :: getMinimumGrade() const{
    return this->_minimumGrade;
}


vector<Student *>& Course :: getStudents() {
    return this->_students;
}


void Course :: pushToCourse(Student *ptr_student) {

    this->_students.push_back(ptr_student);

    if (this->getDepartment() != ELECTIVE) {
        ptr_student->incrementUnfinishedSemesterMandatoryCourses();
    } else {
        ptr_student->incrementUnfinishedSemesterElectiveCourses();
    }
}


const string Course :: getName() const {
    return this->_name;
}


const string Course :: getDepartment() const {
    return this->_department;
}
