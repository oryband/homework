#include "../include/Department.h"

Department :: Department(std::string depName){
	this->_name.assign(depname);
}

void Department :: registerStudents(unsigned short semester){

	vector<Student>::iterator student;
	vector<Course>::iterator course;

	// Iterating Autumn Course list
	if (semerster == 1) {

		// All courses at autumn
		for ( course = this->_autumnCourses.begin() ;
				course != this->_autumnCourses.end() ; ++course) {

			// All students at specific autumn course
			for (student = course->students.begin() ;
					student != course->students.end() ; ++ student) {

				course.reg(student); // Registering the student to the course!
			}
		}
	}
	// Iterating Spring Course list
	if (semester == 0) {

		// All courses at spring
		for ( course = this->_springCourses.begin() ;
				course != this->_springCourses.end() ; ++course) {

			// All students in specific autumn course
			for (student = course->students.begin() ;
					student != course->students.end() ; ++ student) {

				course.reg(student); // Registering the student to the course!
			}
		}

	}
}

void Department ::  teach(unsigned short semester) {

	vector<Course>::iterator course;

	// Iterating Autumn Course list and teach!
	if (semerster == 1) {

		for ( course = this->_autumnCourses.begin() ;
				course != this->_autumnCourses.end() ; ++course) {

			course.teach();
		}
	}
	// Iterating Spring Course list and teach!
	if (semester == 0) {

		for ( course = this->_springCourses.begin() ;
				course != this->_springCourses.end() ; ++course) {

			course.teach();
		}
	}
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
