#include "../include/Consts.h"
#include "../include/Uni.h"

#include <typeinfo>

using namespace std;


vector< vector<string> >* Consts :: getLines(string filePath) {

    string line;
    ifstream file;

    file.open(filePath.c_str());

    if (!file) {
        cout << "Unable to open " << filePath <<  endl;
        exit(1);  // Terminate with error.
    }


    vector< vector<string> >* lines = new vector< vector<string> >;

    while (file >> line) {
        vector<string> words;

        int b = 0,  // Begin index.
            e = line.find(',');  // End index.
        while (e != string::npos) {
            words.push_back(line.substr(b, e - b));

            b = e+1;
            e = line.find(',', b);
        }

        words.push_back(line.substr(b, line.size() - b));

        lines->push_back(words);
    }

    file.close();
    return lines;
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
