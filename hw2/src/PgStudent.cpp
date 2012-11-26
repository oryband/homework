#include "PgStudent.h"

#include "Course.h"

#include <time.h>

void PgStudent :: study(Course &course) {
    // If Student finished course succesfully:
    srand(time(NULL));
    if (rand() % GRADE_RANGE < PG_QUIT_CHANCE) {
        writeToStudentsLogFile(
                this->_id,
                course.getName(),
                this->_department,
                SLACKING_COURSE);
    } else if (rand() % GRADE_RANGE < course.getMinimumGrade()) {
        writeToStudentsLogFile(
                this->_id,
                course.getName(),
                this->_department,
                FAILED_COURSE);
    } else {
        this->finishcourse(course);

        writeToStudentsLogFile(
                this->_id,
                course.getName(),
                this->_department,
                FINISHED_COURSE);
    }
}
