#include "CsStudent.h"

#include "Course.h"


void CsStudent :: study(Course &course) {

    int r1 = rand() / (RAND_MAX / (GRADE_RANGE + 1));
    int r2 = rand() / (RAND_MAX / (GRADE_RANGE + 1));

    // If Student finished course succesfully:
    if (r1 < CS_QUIT_CHANCE) {
        writeToStudentsLogFile(
                this->_id,
                course.getName(),
                this->_department,
                QUITS_COURSE);
    } else if (r2 < course.getMinimumGrade()) {
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
