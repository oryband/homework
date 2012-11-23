#include "CsStudent.h"

#include "Course.h"


void CsStudent :: study(Course &course){
    // If Student finished course succesfully:
	if (rand() % GRADE_RANGE >= course.getMinimumGrade() &&
            rand() % GRADE_RANGE >= CS_QUIT_CHANCE) {

        this->finishcourse(course);

        Util::writeToStudentsLogFile(this->_id,
                course.getName(),
                this->_department,
                FINISHED_COURSE);
	} else {
        Util::writeToStudentsLogFile(this->_id,
                course.getName(),
                this->_department,
                FAILED_COURSE);
    }
}


CsStudent:: ~CsStudent() { cout << "CsStudent dead" << endl; }
