#include "../include/PgCourse.h"

virtual void PgCourse :: reg(Student &s){

	// Check if student had finished its previous semester
	if (this->_semester == s._currentSemester &&
			s._unfinishedSemesterCourses == 0) {

		this->students.push_back(s);
	}
}
