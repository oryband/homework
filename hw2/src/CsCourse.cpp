#include "../include/CsCourse.h"

virtual void CsCourse :: reg(Student &s){

	// Check if student had finished its previous semester
	if (this->_semester == s._currentSemester &&
			s._unfinishedSemesterCourses == 0) {

		this->students.push_back(s);

		// PRINT TO random.log  the assigned student whos here
		// STUD-ID "is taking" COURSE-ID "from" DEPARTMENT
	}

}

