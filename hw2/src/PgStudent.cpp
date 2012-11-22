#include "PgStudent.h"

#include "Course.h"


void PgStudent :: study(Course &course) {
    // If Student finished course succesfully:
	if (rand() % GRADE_RANGE >= course.getMinimumGrade() &&
            rand() % GRADE_RANGE >= PG_QUIT_CHANCE) {

		this->finishcourse(course);

        writeToStudentsLogFile(
                this->_id,
                course.getName(),
                this->_department,
                FINISHED_COURSE);
	} else {
        writeToStudentsLogFile(
                this->_id,
                course.getName(),
                this->_department,
                FAILED_COURSE);
    }
}
