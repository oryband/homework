#include "Student.h"


using namespace std;


Student :: Student(
        string id,
        string department,
        string imagePath,
        unsigned short electiveCourses) :

    _id(id),
    _department(department),
    _imagePath(imagePath),
    _unfinishedSemesterCourses(0),
    _unfinishedElectiveCourses(electiveCourses),
    _currentSemester(0) {
}

//FIXME !!!
void Student :: finishcourse(Course &course){

    vector<StudentPointer>::iterator it_student;

    for (it_student = course.getStudents()->begin();
            it_student != course.getStudents()->end(); ++it_student) {

        // Found the Student in the course,increse/decrease the 
        // number of unfinished courses & delete from course.
        StudentPointer stp = *it_student;
        //Student st = *stp;
        string s = stp->getStudentId();
        if (s.compare(
                    this->getStudentId()) == 0) {

            if (course.getCourseDepartment() == CS ||
                    course.getCourseDepartment() == PG) {

                this->_unfinishedSemesterCourses--;
            }
            else {
                this->_unfinishedElectiveCourses--;
            }

            vector<StudentPointer>* v = course.getStudents();
            v->erase(*it_student);
        }
    }
}


unsigned short Student :: getUnfinishedSemesterCourses() {
    return this->_unfinishedSemesterCourses;
}


unsigned short Student :: getUnfinishedElectiveCourses() {
    return this->_unfinishedElectiveCourses;
}


unsigned short Student :: getCurrentSemester() {
    return this->_currentSemester;
}


void  Student :: increaseUnfinishedSemesterCourses() {
    this->_unfinishedSemesterCourses++;
}

void  Student :: decreaseUnfinishedElectiveCourses() {
    this->_unfinishedSemesterCourses--;
}
