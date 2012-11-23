#include "Uni.h"


using namespace std;


Uni :: Uni(bool pgOn) :
    _students(),

    _mandatoryAutumnCourses(),
    _mandatorySpringCourses(),

    _electiveAutumnCourses(),
    _electiveSpringCourses(),

    _semesters(0),

    _pgOn(pgOn),

    _numOfCsStudents(0),
    _numOfPgStudents(0),

    _numOfCsStuInImage(0),
    _numOfPgStuInImage(0) {

        unsigned short
            *numOfCsElectiveCourses = new unsigned short(),
            *numOfPgElectiveCourses = new unsigned short();

        readCurriculumFile(*numOfCsElectiveCourses, *numOfPgElectiveCourses);
        readStudentsFile(*numOfCsElectiveCourses, *numOfPgElectiveCourses);
        readCoursesFile();

        delete numOfCsElectiveCourses;
        delete numOfPgElectiveCourses;
}


Uni :: ~Uni() {
    // Delete all Courses.
    this->deleteCourses(this->_mandatoryAutumnCourses);
    this->deleteCourses(this->_mandatorySpringCourses);
    this->deleteCourses(this->_electiveAutumnCourses);
    this->deleteCourses(this->_electiveSpringCourses);

    // Delete all students.
    std::vector<Student *>::iterator it_student;

    for (it_student = this->_students.begin();
            it_student != this->_students.end();
            ++it_student) {

        delete (*it_student);
        *it_student = 0;
    }
}


void Uni :: readCurriculumFile(
        unsigned short &numOfCsElectiveCourses,
        unsigned short &numOfPgElectiveCourses) {

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
            numOfCsElectiveCourses = electiveCourses;
        } else {  // PG department.
            numOfPgElectiveCourses = electiveCourses;
        }
    }

    delete lines;
}


void Uni :: readStudentsFile(
        unsigned short &csElectiveCourses,
        unsigned short &pgElectiveCourses) {

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
            this->_numOfCsStudents ++;

        } else {  // PG

            ptr_student = new PgStudent(
                    id, imagePath, pgElectiveCourses);

            this->_numOfPgStudents ++;
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
    for (unsigned int l=0; l < length; l++) {

        vector<string> line = (*lines)[l];

        string department = string(line[0]),
               name = string(line[1]);

        unsigned short semester, minimumGrade;

        istringstream oss1(line[3]);
        oss1 >> semester;

        istringstream oss2(line[4]);
        oss2 >> minimumGrade;

        if (department.compare(ELECTIVE) == 0) {
            if (semester % 2 == 1) {  // Autumn elective course.

                this->_electiveAutumnCourses.push_back(
                        new ElCourse(name, semester, minimumGrade));

            } else { // Spring elective course.

                this->_electiveSpringCourses.push_back(
                        new ElCourse(name, semester, minimumGrade));
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
                    new PgCourse(name, semester, minimumGrade));

            } else {  // Spring course.

                this->_mandatorySpringCourses.push_back(
                    new PgCourse(name, semester, minimumGrade));
            }
        }
    }

    delete lines;
}


void Uni :: simulate() {

    // Log to file if MALAG hasn't approved PG department.
    if ( ! this->_pgOn ) {

        vector<Student *>::iterator it_student;

        for (it_student = this->_students.begin();
                it_student != this->_students.end();
                ++it_student) {

            writeToStudentsLogFile(
                    (**it_student).getStudentId(), "", "", DENIED);
        }
    }

    for (unsigned short currentSemester = 1;
            currentSemester <= this->_semesters;
            currentSemester++) {

        // Log semester number.
        writeNumOfSemesterToFile(currentSemester);

        // Registers, teaches and promotes students for this semester.
        this->registerStudentsToCourses(currentSemester);
        this->teach(currentSemester);
        this->promoteStudents();
        this->graduate();
    }
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

    /* Iterate over all students, and register those who finished their
     * last semester succesfully.
     * NOTE: Doesn't register Pg Students to any course if MALAG did not
     * approve their studies. */
    vector<Student *>::iterator it_student;

    for (it_student = this->_students.begin();
            it_student != this->_students.end(); ++it_student) {

        // Register student to next semester only if he finished last one
        // successfuly.
        if ((**it_student).getUnfinishedSemesterMandatoryCourses() == 0) {

            // Register if CS student,
            // OR if MALAG is on and PG student.
            if ( (**it_student).getDepartment().compare(CS) == 0 ||
                    ! this->_pgOn) {

                registerStudentToMandatoryCourses(
                        *mandatorySemesterCourses, **it_student);

                // If student needs to register to elective courses, do so.
                if ((**it_student).getNecessaryElectiveCourses() > 0) {

                    registerStudentToElectiveCourses(
                            *electiveSemesterCourses, **it_student);
                }
            }
        }
    }
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


void Uni :: graduate() {

    sort(
            this->_students.begin(),
            this->_students.end(),
            compareStudents());

    /*ImageLoader
        csGraduationImage(
            PROFILE_IMAGE_SIZE,
            numOfStudents*PROFILE_IMAGE_SIZE),
        pgGraduationImage(
            PROFILE_IMAGE_SIZE,
            numOfStudents*PROFILE_IMAGE_SIZE);*/

    /* Iterate all students in vector,
     * log their graduations status to file,
     * and generate their profile image. */
    vector<Student *>::iterator it_student;

    for (it_student = this->_students.begin();
            it_student != this->_students.end(); ++it_student) {

        // If student has graduated succesfully:
        if ((**it_student).getUnfinishedSemesterMandatoryCourses() == 0 &&
                (**it_student).getNecessaryElectiveCourses() == 0 &&
                (**it_student).getCurrentSemester() == _semesters -1) {

            // Log to file.
            writeToStudentsLogFile(
                    (**it_student).getStudentId(), "", "", GRADUATED); 

            /*if ((**it_student).getDepartment() == CS) {
                saveColorImage(csGraduationImage, **it_student); 
            } else {
                saveColorImage(pgGraduationImage, **it_student); 
            }*/
        } else {

            // Log to file.
            writeToStudentsLogFile(
                    (**it_student).getStudentId(), "", "", NOT_GRADUATED);

            /*if ((**it_student).getDepartment() == CS) {
                saveGreyscaleImage(csGraduationImage, **it_student); 
            } else {
                saveGreyscaleImage(pgGraduationImage, **it_student); 
            }*/
        }
    }

    // TODO save images on root project folder
}


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


/*void Uni :: saveColorImage(ImageLoader &image, Student& student) {

    ImageOperations opr;

    ImageLoader studentImg(student.getImagePath());
    ImageLoader studentImgResized(100,100);

    opr.resize(studentImg.getImage(), studentImgResized.getImage());

    if (student.getDepartmentName().compare(CS) == 0) {

        opr.copy_paste_image(studentImgResized.getImage(),
                this->_csPicture.getImage(),
                this->_numOfCsStuInImage*100);
        this->_numOfCsStuInImage++;
    } else {
        opr.copy_paste_image(studentImgResized.getImage(),
                this->_pgPicture.getImage(),
                this->_numOfPgStuInImage*100);
        this->_numOfPgStuInImage++;
    }
}*/


/*void Uni :: saveGreyscaleImage(ImageLoader &image, Student& student) {

    ImageOperations opr;

    ImageLoader studentImg(student.getImagePath());
    ImageLoader studentImgResized(100,100);

    opr.resize(studentImg.getImage(), studentImgResized.getImage());

    opr.rgb_to_greyscale(studentImgResized.getImage(),
            studentImgResized.getImage()); //working! checked!


            if (student.getDepartmentName().compare(CS) == 0) {

            opr.copy_paste_image(studentImgResized.getImage(),
                this->_csPicture.getImage(),
                this->_numOfCsStuInImage*100);
            this->_numOfCsStuInImage++;
            } else {

            opr.copy_paste_image(studentImgResized.getImage(),
                this->_pgPicture.getImage(),
                this->_numOfPgStuInImage*100);
            this->_numOfPgStuInImage++;
            }
}*/


void Uni :: deleteCourses(vector<Course *> &courses) {

    std::vector<Course *>::iterator it_course;

    for (it_course = courses.begin();
            it_course != courses.end();
            ++it_course) {

        delete *it_course;
        *it_course = 0;
    }
}
