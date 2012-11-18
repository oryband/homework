#include "Uni.h"


using namespace std;


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


void Consts :: readCoursesFile(string coursesPath, Department &cs,
												   Department &pg,
												   Department &el) {

    vector< vector<string> >* lines = getLines(coursesPath);


    // Iterate over lines and copy data.
    size_t length = lines->size();
    for(unsigned int l=0; l < length; l++) {

        vector<string> line = (*lines)[l];  // Get line

        // Fetch words and cast to appropriate type.
		string _departmentName;
		string _courseName;
		unsigned short _activAtSemester;
		unsigned short _minGrade;

		istringstream oss1(line[0]);  // Cast department day.
		oss1 >> _departmentName;

		istringstream oss2(line[1]);  // Cast department day.
		oss2 >> _courseName;

		istringstream oss3(line[2]);  // Cast department day.
		oss3 >> _activAtSemester;

		istringstream oss4(line[3]);  // Cast department day.
		oss4 >> _minGrade;

        if( _departmentName.compare("CS") == 0 ) {

        	if ( _activAtSemester%(2) == 1 ) {   //  Its autumn course

        		cs._autumnCourses.push_back(*new CsCourse(_courseName,_activAtSemester,_minGrade));
        	}
        	else {						//  Its spring course
        		cs._springCourses.push_back(*new CsCourse(_courseName,_activAtSemester,_minGrade));
        	}
        }
        if( _departmentName.compare("PG") == 0 ) {

			if ( _activAtSemester%(2) == 1 ) {   //  Its autumn course

				pg._autumnCourses.push_back(*new PgCourse(_courseName,_activAtSemester,_minGrade));
			}
			else {						//  Its spring course
				pg._springCourses.push_back(*new PgCourse(_courseName,_activAtSemester,_minGrade));
			}
		}
        if( _departmentName.compare("ELECTIVE") == 0 ) {

			if ( _activAtSemester%(2) == 1 ) {   //  Its autumn course

				el._autumnCourses.push_back(*new ElCourse(_courseName,_activAtSemester,_minGrade));
			}
			else {						//  Its spring course
				el._springCourses.push_back(*new ElCourse(_courseName,_activAtSemester,_minGrade));
			}
		}
    }
}


void Uni :: registerStudentsToCourses(unsigned short currentSemester) {

    vector<Course> *mandatorySemesterCourses, *electiveSemesterCourses;

    if (currentSemester % 2 == 1) {  // Autumn semester.
        mandatorySemesterCourses = &(this->_mandatoryAutumnCourses);
        electiveSemesterCourses = &(this->_electiveAutumnCourses);
    } else {  // Spring Semester
        mandatorySemesterCourses = &(this->_mandatorySpringCourses);
        electiveSemesterCourses = &(this->_electiveSpringCourses);
    }

    // Iterate over all students, and register those who finished their
    // last semester succesfully.
	vector<Student>::iterator it_student;

    for (it_student = this->_students.begin();
            it_student != this->_students.end(); ++it_student) {
        
        if (it_student->getUnfinishedSemesterCourses() == 0) {
            registerStudentToMandatoryCourses(
                    *mandatorySemesterCourses, *it_student);
        }

        // If student needs to register to elective courses, do so.
        if (it_student->getUnfinishedElectiveCourses() > 0) {
            registerStudentToElectiveCourses(
                    *electiveSemesterCourses, *it_student);

        }
    }
}


void Uni :: registerStudentToMandatoryCourses(
        vector<Course> &mandatorySemesterCourses, Student &student) {

	vector<Course>::iterator it_mandatoryCourse;

    for (it_mandatoryCourse = mandatorySemesterCourses.begin();
            it_mandatoryCourse != mandatorySemesterCourses.end();
            ++it_mandatoryCourse) {

        // TODO: Check if this condition is even necessary.
        if ( ! studentInCourse(*it_mandatoryCourse, student) ) {
            it_mandatoryCourse->reg(student);
        }
    }
}


void Uni :: registerStudentToElectiveCourses(
        vector<Course> &electiveSemesterCourses, Student &student) {

	vector<Course>::iterator it_electiveCourse;

    for (it_electiveCourse = electiveSemesterCourses.begin();
            it_electiveCourse != electiveSemesterCourses.end();
            ++it_electiveCourse) {

        // Only register if student isn't already registered.
        if ( ! studentInCourse(*it_electiveCourse, student) ) {

            it_electiveCourse->reg(student);
        }
    }
}


unsigned short Uni :: getUnfinishedSemesterCourses() {
    return this->_unfinishedSemesterCourses;
}


unsigned short Uni :: getUnfinishedElectiveCourses() {
    return this->_unfinishedElectiveCourses;
}


/**
 * Returns true if student is already registered to course.
 */
bool Uni :: studentInCourse(Course &course, Student &student) {
    vector<Student>::iterator it_student;
    for (it_student = course.begin(); it_student != course.end();
            ++it_student) {

        if (compare(it_student.getStudentId(), s.id) == 0) {
            return true;
        }
    }

    return false;
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

void Consts :: readStudentsFile(string studentPath,Department &cs,
												 Department &pg,
												 Department &el) { // to fix
	vector< vector<string> >* lines = getLines(studentPath);


	    // Iterate over lines and copy data.
	    size_t length = lines->size();
	    for(unsigned int l=0; l < length; l++) {

	        vector<string> line = (*lines)[l];  // Get line

	        // Fetch words and cast to appropriate type.
			string _departmentName;
	        string _stuId;
			string _imgPath;

			istringstream oss1(line[0]);  // Cast department day.
			oss1 >> _stuId;

			istringstream oss2(line[1]);  // Cast department day.
			oss2 >> _departmentName;

			istringstream oss3(line[2]);  // Cast department day.
			oss3 >> _imgPath;


	        if( _departmentName.compare("CS") == 0 ) {

	        	cs._students.push_back(*new CsStudent(_stuId, _imgPath));
	        }

	        if( _departmentName.compare("PG") == 0 ) {

	        	pg._students.push_back(*new PgStudent(_stuId, _imgPath));

			}
	    }
}

void Consts :: readCurriculumFile(string curriculumPath, Department &cs,
														 Department &pg,
														 Uni &u){

	vector< vector<string> >* lines = getLines(curriculumPath);

			vector<string> line = (*lines)[0];  // Get line

			// Fetch the number of semester
			string _numberOfSemesters;

			istringstream oss1(line[0]);  // Cast department day.
			oss1 >> _numberOfSemesters;

			//here 19:34 need to finish convert the char to unsigned int!!!!
			u.setNumberOfSemeter( (unsigned int)_numberOfSemesters[20]);


		    // Iterate over lines and copy data.
		    size_t length = lines->size();
		    for(unsigned int l=1; l < length; l++) {

		        vector<string> line = (*lines)[l];  // Get line

		        // Fetch words and cast to appropriate type.
		        string _departmentName;
		        string _numManElective;

				istringstream oss2(line[0]);  // Cast department day.
				oss2 >> _departmentName;

				istringstream oss3(line[1]);  // Cast number of electives.
				oss3 >> _numManElective;

		        if( _departmentName.compare("CS") == 0 ) {

		        	cs.setMandatoryElectiveCourses( atoi(_numManElective.c_str()) );
		        }
		        else {

		        	pg.setMandatoryElectiveCourses( atoi(_numManElective.c_str()) );
		        }
		    }
}
