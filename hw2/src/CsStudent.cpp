#include "CsStudent.h"

#include "Course.h"

#include <time.h>

using namespace std;

void CsStudent :: study(Course &course) {
    // If Student finished course succesfully:
    srand(time(NULL));
    if (rand() % GRADE_RANGE < CS_QUIT_CHANCE) {
        writeToStudentsLogFile(
                this->_id,
                course.getName(),
                this->_department,
                QUITS_COURSE);
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
