#include "Student.h"


void Studnet :: finishcourse(Course &c){

	this->_unfinishedSemesterCourses -= 1;
	c.erase(this); // copy constructor??
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
