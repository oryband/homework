#include "Student.h"

#include "Course.h"


using namespace std;


Student :: Student(
        string id,
        string imagePath,
        unsigned short electiveCourses,
        string department) :

    _id(id),
    _department(department),
    _imagePath(imagePath),
    _currentSemester(0),
    _unfinishedSemesterMandatoryCourses(0),
    _unfinishedSemesterElectiveCourses(0),
    _necessaryElectiveCourses(electiveCourses) {}


void Student :: finishcourse(Course &course) {

    if (course.getDepartment() != _ELECTIVE_) {  // CS or PG course.
        this->_unfinishedSemesterMandatoryCourses --;
    } else {  // Elective course.
        this->_unfinishedSemesterElectiveCourses --;
        this->_necessaryElectiveCourses --;
    }

    // Remove student from course.
    vector<Student *>::iterator it_student;
    if (course.getStudents().size() > 0) {
        for (it_student = course.getStudents().begin();
                it_student != course.getStudents().end(); ++it_student) {

            if ((**it_student).getId().compare(
                        this->getId()) == 0) {

                course.getStudents().erase(it_student);
                return;
            }
        }
    }
}


const string Student :: getId() const {
    return this->_id;
}


const string Student :: getDepartment() const {
    return this->_department;
}



const string Student :: getImagePath() const {
    return this->_imagePath;
}
/*const string Student :: getImagePath() const {
    return this->_imagePath;
}*/


unsigned short Student :: getCurrentSemester() const {
    return this->_currentSemester;
}


unsigned short Student :: getUnfinishedSemesterMandatoryCourses() const {
    return this->_unfinishedSemesterMandatoryCourses;
}


unsigned short Student :: getUnfinishedSemesterElectiveCourses() const {
    return this->_unfinishedSemesterElectiveCourses;
}


unsigned short Student :: getNecessaryElectiveCourses() const {
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
