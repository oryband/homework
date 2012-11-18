#include "../include/Department.h"

Department :: Department(std::string depName){
	this->_name.assign(depname);
}


void Department :: gruduate(unsigned short numOfsemesters) {

	vector<Student>::iterator student;

	// All students in department
		for (student = this->_students.begin() ;
				student != this->_students.end()  ; ++ student) {

			if ( student->_currentSemester == numOfsemesters &&
					student->_unfinishedSemesterCourses == 0 &&
					student->_electiveCoursesUnfinished == 0 ) {

				// NEED TO GENERATE COLOR IMAGE!!!!! read
				// And save CS.jpg/PG.jpg into root
			}
			else {
				// NEED TO GENERATE GRAY SCALE IMAGE!!!!!
				// And save CS.jpg/PG.jpg into root
			}
		}
}

void Department :: setMandatoryElectiveCourses(unsigned short mandatoryNum){
	this->_mandatoryElectiveCourses = mandatoryNum;
}
