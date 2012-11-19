#include "Course.h"


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

        this->teachStudent(**it_student);  // TODO Implement in child classes.
	}
}



unsigned short Course :: getMinimumGrade() const{
    return this->_minimumGrade;
}


const vector<Student *>& Course :: getStudents() {
    return this->_students;
}


void  Course :: pushToCourse(Student *ptr_student) {

    this->_students.push_back(ptr_student);
    
    if (this->getCourseDepartment() == CS ||
            this->getCourseDepartment() == PG) {

        ptr_student->increaseUnfinishedSemesterCourses();
    }
}


string Course :: getCourseName() const {
    return this->_name;
}


string Course :: getCourseDepartment() const {
    return this->_department;
}
