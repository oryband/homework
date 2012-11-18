#include "../include/CsCourse.h"

CsCourse :: CsCourse( std::string courseName,unsigned short semesterNum,
								   unsigned short minimumGrade ){

	this->_courseName.assign(courseName);
	this->_semester = semesterNum;
	this->_minimumGrade = minimumGrade;
}

//  Huge issue with logic..need to clear it out!!!
virtual void CsCourse :: reg(Student &s){

	// Check if student had finished its previous semester
	if (this->_semester == s._currentSemester &&
			s._unfinishedSemesterCourses == 0) {

		this->students.push_back(s);

		// PRINT TO random.log  the assigned student who is here
		// STUD-ID "is taking" COURSE-ID "from" DEPARTMENT
	}
	if ( this->_semester == s._currentSemester &&){

	}

}

