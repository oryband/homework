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
	vector<Student *>::reverse_iterator it_student;

	for (it_student = this->_students.rbegin();
            it_student != this->_students.rend(); ++it_student) {

        (**it_student).study(*this);
	}
}


unsigned short Course :: getMinimumGrade() const{
    return this->_minimumGrade;
}


vector<Student *>& Course :: getStudents() {
    return this->_students;
}


void Course :: pushToCourse(Student *ptr_student) {

    this->_students.push_back(ptr_student);

    if (this->getDepartment() != _ELECTIVE_) {  // Mandatory course.
        ptr_student->incrementUnfinishedSemesterMandatoryCourses();
    } else {  // Elective course.
        ptr_student->incrementUnfinishedSemesterElectiveCourses();
    }
}


const string Course :: getName() const {
    return this->_name;
}


const string Course :: getDepartment() const {
    return this->_department;
}


unsigned short Course :: getSemester() const {
    return this->_semester;
}
