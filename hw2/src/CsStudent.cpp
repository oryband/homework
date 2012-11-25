#include "CsStudent.h"

#include "Course.h"

using namespace std;

void CsStudent :: study(Course &course){
    cout << "Course: " << course.getName() << endl;
    // If Student finished course succesfully:
	if (rand() % GRADE_RANGE >= course.getMinimumGrade() &&
            rand() % GRADE_RANGE >= CS_QUIT_CHANCE) {

        cout << "\t" << this->_id << ": finished" << endl;
        this->finishcourse(course);
        cout << "\t\tremoved myself from course" << endl;

        /*writeToStudentsLogFile(
                this->_id,
                course.getName(),
                this->_department,
                FINISHED_COURSE);*/
	} else {
        cout << "\t" << this->_id << ": UNFINISHED" << endl;
        /*writeToStudentsLogFile(
                this->_id,
                course.getName(),
                this->_department,
                FAILED_COURSE);*/
    }
    cout << "\t" << this->_id << ": end of study" << endl;
}
