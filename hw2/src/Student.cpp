#include "../include/Student.h"

void Studnet :: finishcourse(Course &c){

	this->_unfinishedSemesterCourses -= 1;
	c.erase(this); // copy constructor??
}
