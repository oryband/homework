#include "../include/ElCourse.h"

ElCourse :: ElCourse( std::string courseName,unsigned short semesterNum,
								   unsigned short minimumGrade ){

	this->_courseName.assign(courseName);
	this->_semester = semesterNum;
	this->_minimumGrade = minimumGrade;
}
