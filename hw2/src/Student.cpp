#include "Student.h"

#include "Course.h"


using namespace std;


Student :: Student(
        string id,
        string department,
        string imagePath,
        unsigned short electiveCourses) :

    _id(id),
    _department(department),
    _imagePath(imagePath),
    _currentSemester(0),
    _unfinishedSemesterMandatoryCourses(0),
    _unfinishedSemesterElectiveCourses(0),
    _necessaryElectiveCourses(electiveCourses) {}


void Student :: finishcourse(Course &course) {

    if (course.getDepartment() != ELECTIVE) {  // CS or PG course.
        this->_unfinishedSemesterMandatoryCourses --;
    } else {  // Elective course.
        this->_unfinishedSemesterElectiveCourses --;
        this->_necessaryElectiveCourses --;
    }

    // Remove student from course.
    vector<Student *>::iterator it_student;
    for (it_student = course.getStudents().begin();
            it_student != course.getStudents().end(); ++it_student) {

        if ((**it_student).getStudentId().compare(
                    this->getStudentId()) == 0) {

            course.getStudents().erase(it_student);
            return;
        }
    }
}


const string Student :: getStudentId() const {
    return this->_id;
}


const string Student :: getDepartment() const {
    return this->_department;
}


const string Student :: getImagePath() const {
    return this->_imagePath;
}


const unsigned short Student :: getCurrentSemester() const {
    return this->_currentSemester;
}


const unsigned short Student :: getUnfinishedSemesterMandatoryCourses() const {
    return this->_unfinishedSemesterMandatoryCourses;
}


const unsigned short Student :: getUnfinishedSemesterElectiveCourses() const {
    return this->_unfinishedSemesterElectiveCourses;
}


const unsigned short Student :: getNecessaryElectiveCourses() const {
    return this->_necessaryElectiveCourses;
}


void Student :: incrementUnfinishedSemesterMandatoryCourses() {
    this->_unfinishedSemesterMandatoryCourses ++;
}


void Student :: incrementUnfinishedSemesterElectiveCourses() {
    this->_unfinishedSemesterElectiveCourses ++;
}


void Student :: promoteToNextSemster() {
    this->_currentSemester ++;    
}
