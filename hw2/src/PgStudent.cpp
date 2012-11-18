#include "../include/PgStudent.h"

PgStudent :: PgStudent(string studentId, String imagePath){

		_studentId.assign(studentId);
		_imagePath.assign(imagePath);
		_unfinishedSemesterCourses = 0;
		_electiveCoursesUnfinished = 0;

}

void PgStudent :: study(Course &c){

	if ( rand()%100 >= c.getMinGrade() && rand()%100 >= 20 ) {

		this->finishCourse();
	}

}
