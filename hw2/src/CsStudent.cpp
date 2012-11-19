#include "CsStudent.h"

void CsStudent :: study(Course &c){

	if (rand()%100 >= c.getMinimumGrade() && rand()%100 >= 25) {

		this->finishcourse(c);
        writeToFileStudents(this->_id, c.getCourseName(), "", 2);
	}
    else {

        writeToFileStudents(this->_id, c.getCourseName(), "", 3);
    }
}
