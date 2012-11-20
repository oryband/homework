#include "Uni.h"


using namespace std;
using namespace boost;


Uni :: Uni(bool pgOn) :
        _semesters(0),

        _pgOn(pgOn),

        _CsNumOfElctiveCourses(0),
        _PgNumOfElctiveCourses(0),

        _students(),

        _mandatoryAutumnCourses(),
        _mandatorySpringCourses(),

        _electiveAutumnCourses(),
         _electiveSpringCourses() {

    readCurriculumFile();

    readStudentsFile(
            this->_CsNumOfElctiveCourses,
            this->_PgNumOfElctiveCourses);

	readCoursesFile();
}


void Uni :: simulate() {

    for (unsigned short currentSemester = 1;
            currentSemester <= this->_semesters;
            currentSemester++) {

        //  Write to head of file random.log - Semster Title
        writeToFileNumOfSemester(currentSemester);
        
        // Registers, teaches and promotes students for this semester.
        this->registerStudentsToCourses(currentSemester);
        this->teach(currentSemester);
        this->promoteStudents();
        // TODO Graduate().
    }
}


void Uni :: promoteStudents() {

    vector<Student *>::iterator it_student;
    vector<Course *>::iterator it_course;

    for (it_student = this->_students.begin();
            it_student != this->_students.end(); ++it_student) {

        if ((**it_student).getUnfinishedSemesterMandatoryCourses() == 0) {

            (**it_student).promoteToNextSemster();
        }
    }   
}


void Uni :: readCurriculumFile() {

    vector< vector<string> > *lines = new vector< vector<string> >;
    getLines(CURRICULUM_FILE, *lines);

    vector<string> line = (*lines)[0];

    // Read number of semesters.
    string numberOfSemesters;
    int seperatorIndex = line[0].find('=');

    // Cast to unsigned short.
    istringstream oss1(
            numberOfSemesters.substr(
                seperatorIndex,
                numberOfSemesters.size() - seperatorIndex));

    oss1 >> this->_semesters;


    // Read department name, and amount of necessary elective courses each
    // student must take for each department.
    size_t length = lines->size();
    for (unsigned int l=1; l < length; l++) {

        vector<string> line = (*lines)[l];

        string department(line[0]),
               unfinishedElectiveCourses;

        istringstream oss2(line[1]);  // Cast number of electives.
        oss2 >> unfinishedElectiveCourses;

        int electiveCourses = atoi(unfinishedElectiveCourses.c_str());
        if (department.compare(CS) == 0) {  // CS department.
            this->_CsNumOfElctiveCourses = electiveCourses;
        } else {  // PG department.
            this->_PgNumOfElctiveCourses = electiveCourses;
        }
    }

    delete lines;
}


void Uni :: readStudentsFile(
        unsigned short csElectiveCourses,
        unsigned short pgElectiveCourses) {

    vector< vector<string> > *lines = new vector< vector<string> >;
    getLines(STUDENTS_FILE, *lines);

    // Iterate over lines and copy data.
    size_t length = lines->size();
    for (unsigned int l=0; l < length; l++) {

        vector<string> line = (*lines)[l];

        string id = string(line[0]),
               imagePath = string(line[1]),
               department = string(line[2]);

        Student *ptr_student;
        if (department.compare(CS) == 0) {

            ptr_student = new CsStudent(
                    id, imagePath, csElectiveCourses);

        } else {  // PG

            ptr_student = new PgStudent(
                    id, imagePath, pgElectiveCourses);
        }

        this->_students.push_back(ptr_student);
    }

    delete lines;
}


void Uni :: readCoursesFile() {

    vector< vector<string> > *lines = new vector< vector<string> >;
    getLines(COURSES_FILE, *lines);

    // Iterate over lines and copy data.
    size_t length = lines->size();
    for(unsigned int l=0; l < length; l++) {

        vector<string> line = (*lines)[l];

        string department = string(line[0]),
               name = string(line[1]);

        unsigned short semester,
                       minimumGrade;

        istringstream oss1(line[3]);
        oss1 >> department;

        istringstream oss2(line[4]);
        oss2 >> name;


        if (department.compare(ELECTIVE) == 0) {

            if (semester % 2 == 1) {  // Autumn elective course.

                this->_electiveAutumnCourses.push_back(
                        new ElCourse(name, semester, minimumGrade));


            } else { // Spring elective course.

                this->_electiveSpringCourses.push_back(
                        new ElCourse (name, semester, minimumGrade));
            }
        } else if (department.compare(CS) == 0) {

            if (semester % 2 == 1) {  // Autumn mandatory course.

                this->_mandatoryAutumnCourses.push_back(
                        new CsCourse(name, semester, minimumGrade));

            } else {  // Spring mandatory course.

                this->_mandatorySpringCourses.push_back(
                        new CsCourse(name, semester, minimumGrade));

            }
        } else if (name.compare(PG) == 0) {

            if (semester % 2 == 1) {  // Autumn course.

                this->_mandatoryAutumnCourses.push_back(
                        new PgCourse(
                            name, semester, minimumGrade));

            } else {  // Spring course.

                this->_mandatorySpringCourses.push_back(
                        new PgCourse (
                            name, semester, minimumGrade));
            }
        }
    }

    delete lines;
}


void Uni :: registerStudentsToCourses(unsigned short currentSemester) {

    vector<Course *> *mandatorySemesterCourses, *electiveSemesterCourses;

    if (currentSemester % 2 == 1) {  // Autumn semester.
        mandatorySemesterCourses = &(this->_mandatoryAutumnCourses);
        electiveSemesterCourses = &(this->_electiveAutumnCourses);
    } else {  // Spring Semester
        mandatorySemesterCourses = &(this->_mandatorySpringCourses);
        electiveSemesterCourses = &(this->_electiveSpringCourses);
    }

    // Iterate over all students, and register those who finished their
    // last semester succesfully.
    vector<Student *>::iterator it_student;

    for (it_student = this->_students.begin();
            it_student != this->_students.end(); ++it_student) {

        if ((**it_student).getUnfinishedSemesterMandatoryCourses() == 0) {

            registerStudentToMandatoryCourses(
                    *mandatorySemesterCourses, **it_student);
        }

        // If student needs to register to elective courses, do so.
        if ((**it_student).getNecessaryElectiveCourses() > 0) {

            registerStudentToElectiveCourses(
                    *electiveSemesterCourses, **it_student);
        }
    }
}


void Uni :: registerStudentToMandatoryCourses(
        vector<Course *> &mandatorySemesterCourses,
        Student &student) {

    vector<Course *>::iterator it_mandatoryCourse;

    for (it_mandatoryCourse = mandatorySemesterCourses.begin();
            it_mandatoryCourse != mandatorySemesterCourses.end();
            ++it_mandatoryCourse) {

        (**it_mandatoryCourse).reg(student);
    }
}


void Uni :: registerStudentToElectiveCourses(
        vector<Course *> &electiveSemesterCourses,
        Student &student) {

    vector<Course *>::iterator it_electiveCourse;

    for (it_electiveCourse = electiveSemesterCourses.begin();
            it_electiveCourse != electiveSemesterCourses.end();
            ++it_electiveCourse) {

        // Register student to minimum number of elective courses necessary to graduate.
        if (
                student.getUnfinishedSemesterElectiveCourses() <
                student.getNecessaryElectiveCourses()) {

            if ( ! isStudentInCourse(**it_electiveCourse, student) ) {

                (**it_electiveCourse).reg(student);
            }
        } else {
            return;
        }
    }
}


/**
 * Returns true if student is already registered to course.
 */
const bool Uni :: isStudentInCourse(Course &course, Student &student) const {

    vector<Student *>::iterator it_student;
    for (it_student = course.getStudents().begin();
            it_student != course.getStudents().end(); ++it_student) {

        if ((**it_student).getStudentId().compare(
                    student.getStudentId()) == 0) {

            return true;
        }
    }
    return false;
}


void Uni :: teach(unsigned short currentSemester) {

    vector<Course *> *mandatorySemesterCourses, *electiveSemesterCourses;
	vector<Course *>::iterator it_mandatoryCourse, it_electiveCourse;

    if (currentSemester % 2 == 1) {  // Autumn semester.
        mandatorySemesterCourses = &(this->_mandatoryAutumnCourses);
        electiveSemesterCourses = &(this->_electiveAutumnCourses);
    }

    for (it_mandatoryCourse = mandatorySemesterCourses->begin();
            it_mandatoryCourse != mandatorySemesterCourses->end();
            ++it_mandatoryCourse) {

        (**it_mandatoryCourse).teach();
    }

    for (it_electiveCourse = electiveSemesterCourses->begin();
            it_electiveCourse != electiveSemesterCourses->end();
            ++it_electiveCourse) {

        (**it_electiveCourse).teach();
    }

    mandatorySemesterCourses = 0;
    electiveSemesterCourses = 0;
}


void Uni :: generateGraduationImage(
        vector<Student *> &students) {

    // FIXME
    sort(students.begin(), students.end(), CompareStudentsFunctor);
    
    // Iterate all students in vector and printing
    vector<Student *>::iterator it_student;

    for (it_student = students.begin();
            it_student != students.end(); ++it_student) {

        if ((**it_student).getUnfinishedSemesterMandatoryCourses() == 0 &&
            (**it_student).getNecessaryElectiveCourses() == 0 &&
            (**it_student).getCurrentSemester() == _semesters) {
            
            //  Write to random.log file - Student graduated
            writeToFileStudents((**it_student).getStudentId(),
                                 "", "", 4);
            SaveColorImage(**it_student);  // TODO

        } else {

            writeToFileStudents((**it_student).getStudentId(),
                                 "", "", 5);
            SaveGreyscaleImage(**it_student);  // TODO
        }
    }
}


void Uni :: deleteVectorCourses(vector<Course *>* vectorCourses) {

    std::vector<Course *>::iterator it_course;

    for (it_course = vectorCourses->begin();
            it_course != vectorCourses->end();
            ++it_course) {

        delete (*it_course);
        (*it_course) = 0;
    }
}


Uni :: ~Uni() {

    //  Delete all Courses vectors 
    this->deleteVectorCourses(&_mandatoryAutumnCourses);
    this->deleteVectorCourses(&_mandatorySpringCourses);
    this->deleteVectorCourses(&_electiveAutumnCourses);
    this->deleteVectorCourses(&_electiveSpringCourses);

    //  Delete students vector
    std::vector<Student *>::iterator it_student;

    for (it_student = this->_students.begin();
            it_student != this->_students.end();
            ++it_student) {

        delete (*it_student);
        *it_student = 0;
    }
}
