#ifndef CONSTS_H_
#define CONSTS_H_

#include "Department.h"

#include <iostream>
#include <fstream>
#include <sstream>
#include <iomanip>
#include <cstdlib>
#include <string>
#include <vector>
#include <math.h>

//#define COURSES_FILE "courses.conf"

class Consts {

public:
	std::vector< std::vector<std::string> >* getLines(std::string filePath);


	void readCoursesFile(std::string coursesPath, Department &cs,
									  	     Department &pg,
										     Department &el);
	/*
	void readStudentsFile(string studentPath, Department &cs,
									  		  Department &pg,
											  Department &el);
*/
};
#endif
