#include "../include/CsStudent.h"

CsStudent :: CsStudent(string studentId, String imagePath){


		this->_studentId.assign(studentId);
		this->_imagePath.assign(imagePath);
		this->_unfinishedSemesterCourses = 0;
		this->_electiveCoursesUnfinished = 0;

}

virtual void CsStudent :: study(Course &c){

	if ( rand()%100 >= c.getMinGrade() && rand()%100 >= 25 ) {


		this->finishCourse();
}

}
