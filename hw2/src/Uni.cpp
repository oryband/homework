#include "Uni.h"


using namespace std;

Uni :: Uni(bool flag) {

    // Read curriculm.conf and assign data
    readCurriculumFile();

	// Read students.conf and assign data
    readStudentsFile(this->_CsNumOfElctiveCourses, 
                     this-> _PgNumOfElctiveCourses);

	// Read courses.conf and assign data
	readCoursesFile();



	if (flag) { // according to plan
    

	}
	else { // PG department don't register and prints to file


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


void Uni :: readCurriculumFile() {

	vector< vector<string> >* lines = getLines(CURRICULUM_FILE);

			vector<string> line = (*lines)[0];  // Get line

			// Fetch the number of semester
			string numberOfSemesters;

			istringstream oss1(line[0]);  // Cast department day.
			oss1 >> numberOfSemesters;

			this->_semesters =(unsigned short)numberOfSemesters[20];

		    // Iterate over lines and copy data.
		    size_t length = lines->size();
		    for(unsigned int l=1; l < length; l++) {

		        vector<string> line = (*lines)[l];  // Get line

		        // Fetch words and cast to appropriate type.
		        string departmentName, numManElective;

				istringstream oss2(line[0]);  // Cast department day.
				oss2 >> departmentName;

				istringstream oss3(line[1]);  // Cast number of electives.
				oss3 >> numManElective;

		        if(departmentName.compare(CS) == 0 ) {

		        	this->_CsNumOfElctiveCourses = 
                        atoi(numManElective.c_str());
		        }
		        else {

		        	this->_PgNumOfElctiveCourses = 
                        atoi(numManElective.c_str());
		        }
		    }
}

void Uni :: readStudentsFile(
        unsigned short CsNumOfElc,
        unsigned short PgNumOfElc) {

	vector< vector<string> >* lines = getLines(STUDENTS_FILE);


	    // Iterate over lines and copy data.
	    size_t length = lines->size();
	    for (unsigned int l=0; l < length; l++) {

	        vector<string> line = (*lines)[l];  // Get line

	        // Fetch words and cast to appropriate type.
			string departmentName, stuId, imgPath;

			istringstream oss1(line[0]);  // Cast department day.
			oss1 >> stuId;

			istringstream oss2(line[1]);  // Cast department day.
			oss2 >> departmentName;

			istringstream oss3(line[2]);  // Cast department day.
			oss3 >> imgPath;

            StudentPointer s;
	        if (dbepartmentName.compare(CS) == 0) {

                s = StudentPointer(
                        new CsStudent(stuId, imgPath, CsNumOfElc));

	        } else {  // PG

                s = StudentPointer(
                        new PgStudent(stuId, imgPath, PgNumOfElc));
            }

            this->_students.push_back(s);
	    }
}

void Uni :: readCoursesFile() {

    vector< vector<string> >* lines = getLines(COURSES_FILE);


    // Iterate over lines and copy data.
    size_t length = lines->size();
    for(unsigned int l=0; l < length; l++) {

        vector<string> line = (*lines)[l];  // Get line

        // Fetch words and cast to appropriate type.
		string departmentName, courseName;
		unsigned short activAtSemester;
		unsigned short minGrade;

		istringstream oss1(line[0]);  // Cast department day.
		oss1 >> departmentName;

		istringstream oss2(line[1]);  // Cast department day.
		oss2 >> courseName;

		istringstream oss3(line[2]);  // Cast department day.
		oss3 >> activAtSemester;

		istringstream oss4(line[3]);  // Cast department day.
		oss4 >> minGrade;


        if(departmentName.compare("ELECTIVE") == 0) {

			if (activAtSemester%(2) == 1) {   //  Its autumn course

		    	this->_electiveAutumnCourses.push_back(
                        *new ElCourse(
                            courseName, activAtSemester, minGrade));
			}
			else {				        		//  Its spring course
				this->_electiveSpringCourses.push_back(*new ElCourse
                        ( courseName, activAtSemester, minGrade));
			}
		}
        else if (departmentName.compare("CS") == 0) {

        	if (activAtSemester%(2) == 1) {   //  Its autumn course

        		this->_mandatoryAutumnCourses.push_back(*new CsCourse
                        (courseName, activAtSemester, minGrade));
        	}
        	else {		        				//  Its spring course
        		this->_mandatorySpringCourses.push_back(*new CsCourse
                        (courseName, activAtSemester, minGrade));
        	}
        }
        else if (departmentName.compare("PG") == 0) {

			if (activAtSemester%(2) == 1) {   //  Its autumn course

				this->_mandatoryAutumnCourses.push_back(*new PgCourse
                        (courseName, activAtSemester, minGrade));
			}
			else {						//  Its spring course
				this->_mandatorySpringCourses.push_back(*new PgCourse
                        (courseName, activAtSemester, minGrade));
			}
		}
    }
}
