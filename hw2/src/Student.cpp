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


void Student :: finishcourse(Course &course){

	course.getStudents().erase(this);  // TODO
	this->_unfinishedSemesterCourses --;
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
