#ifndef UNI_H_
#define UNI_H_

//#define COURSES_FILE "courses.conf"

class Consts {

public:
	vector< vector<string> >* getLines(string filePath);
	void readCoursesFile(string coursesPath, Department &cs,
									  	     Department &pg,
										     Department &el);
	void readStudentsFile(string studentPath, Department &cs,
									  		  Department &pg,
											  Department &el);

};
#endif
