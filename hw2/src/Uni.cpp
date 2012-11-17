#include "../include/Uni.h"

Uni :: Uni(bool flag) {

	Department Cs,Pg,El;

	Consts reader;

	// Read courses.conf and assign data
	vector< vector<string> >* coursesLines = new vector< vector<string> >;

	reader.getLines("../courses.conf", coursesLines);
	reader.readCoursesFile(coursesLines);

	// Read students.conf and assign data
	vector< vector<string> >* studentsLines = new vector< vector<string> >;

	reader.getLines("../students.conf", studentsLines);
    reader.readStudentsFile(studentsLines);

    // Read curriculm.conf and assign data
    vector< vector<string> >* curriculumLines = new vector< vector<string> >;

    reader.getLines("../curriculum.conf", curriculumLines);
    reader.readCurriculumFile(curriculumLines);


	delete coursesLines;
	delete studentsLines;
	delete curriculumLines;


	if (flag) { // according to plan


	}
	else { // PG department don't register and prints to file


	}

}


void Uni :: setNumberOfSemeter(unsigned int numberOfSemesters){

	this->_numOfSemesters = numberOfSemesters;
}

