#include "CsStudent.h"


void CsStudent :: study(Course &course){

	if (rand() % 100 >= course.getMinimumGrade() && rand() % 100 >= 25) {
        this->finishcourse(course);
        writeToFileStudents(this->_id, course.getCourseName(), "", 2);
	} else {
        writeToFileStudents(this->_id, course.getCourseName(), "", 3);
    }
}
