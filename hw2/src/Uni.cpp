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
    _numOfPgStudents(0) {


    unsigned short numOfCsElectiveCourses, numOfPgElectiveCourses;

    readCurriculumFile(numOfCsElectiveCourses, numOfPgElectiveCourses);
    readStudentsFile(numOfCsElectiveCourses, numOfPgElectiveCourses);
    readCoursesFile();

    // Sort students.
    sort(this->_students.begin(), this->_students.end(), compareStudents());

    srand(time(0));  // Seed random generator.
}


Uni :: ~Uni() {
    // Delete all Courses.
    this->deleteCourses(this->_mandatoryAutumnCourses);
    this->deleteCourses(this->_mandatorySpringCourses);
    this->deleteCourses(this->_electiveAutumnCourses);
    this->deleteCourses(this->_electiveSpringCourses);

    // Delete all students.
    vector<Student *>::iterator it_student;

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

    getLines(CURRICULUM_FILE, lines);

    vector<string> line = (*lines)[0];
    // Read number of semesters.
    string numOfSemesters; 
    size_t pos;
    pos = line[0].find("=");

    // Cast to unsigned short.
    istringstream oss1(
            (line[0].substr( pos )).erase(0,1) );

    oss1 >> numOfSemesters;

    this->_semesters = atoi(numOfSemesters.c_str());

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

        if (department.compare(_CS_) == 0) {  // CS department.
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

    getLines(STUDENTS_FILE, lines);

    // Iterate over lines and copy data.
    size_t length = lines->size();
    for (unsigned int l=0; l < length; l++) {

        vector<string> line = (*lines)[l];

        string id = string(line[0]),
               department = string(line[1]),
               imagePath = string(line[2]);

        Student *ptr_student;
        if (department.compare(_CS_) == 0) {
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

    getLines(COURSES_FILE, lines);

    // Iterate over lines and copy data.
    size_t length = lines->size();
    for (unsigned int l=0; l < length; l++) {

        vector<string> line = (*lines)[l];

        string department = string(line[0]),
               name = string(line[1]);

        unsigned short semester, minimumGrade;

        istringstream oss1(line[2]);
        oss1 >> semester;

        istringstream oss2(line[3]);
        oss2 >> minimumGrade;

        if (department.compare(_ELECTIVE_) == 0) {
            if (semester % 2 == 1) {  // Autumn elective course.

                this->_electiveAutumnCourses.push_back(
                        new ElCourse(name, semester, minimumGrade));

            } else { // Spring elective course.

                this->_electiveSpringCourses.push_back(
                        new ElCourse(name, semester, minimumGrade));
            }
        } else if (department.compare(_CS_) == 0) {
            if (semester % 2 == 1) {  // Autumn mandatory course.

                this->_mandatoryAutumnCourses.push_back(
                        new CsCourse(name, semester, minimumGrade));

            } else {  // Spring mandatory course.

                this->_mandatorySpringCourses.push_back(
                        new CsCourse(name, semester, minimumGrade));
            }
        } else {  // PG //if (department.compare(_PG_) == 0) {
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

            if ((**it_student).getDepartment().compare(_PG_) == 0) {
                writeToStudentsLogFile(
                        (**it_student).getId(), "", "", DENIED);
            }
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
    }

    this->graduate();
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
        if ((**it_student).getUnfinishedSemesterMandatoryCourses() == 0 &&
                (**it_student).getUnfinishedSemesterElectiveCourses() == 0) {

            // Register if CS student,
            // OR if MALAG is on and PG studentl.
            if ( (**it_student).getDepartment().compare(_CS_) == 0 ||
                    this->_pgOn) {

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
    } else {  // Spring semester.
        mandatorySemesterCourses = &(this->_mandatorySpringCourses);
        electiveSemesterCourses = &(this->_electiveSpringCourses);
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

    mandatorySemesterCourses = 0;  // FIXME Will removing this cause to destroy Uni's members?
    electiveSemesterCourses = 0;
}


void Uni :: graduate() {

    ImageLoader 
        csGraduationImage(
                PROFILE_IMAGE_SIZE,
                this-> _numOfCsStudents * PROFILE_IMAGE_SIZE),

        pgGraduationImage(
                PROFILE_IMAGE_SIZE,
                this->_numOfPgStudents * PROFILE_IMAGE_SIZE);

    unsigned int numberOfCsImages = 0,
                 numberOfPgImages = 0;
    
    /* Iterate all students in vector,
     * log their graduations status to file,
     * and generate their profile image. */
    vector<Student *>::iterator it_student;

    for (it_student = this->_students.begin();
            it_student != this->_students.end(); ++it_student) {

        // If student has graduated succesfully:
        if ((**it_student).getUnfinishedSemesterMandatoryCourses() == 0 &&
                (**it_student).getNecessaryElectiveCourses() == 0) {
                // TODO Check if this is good.
                //(**it_student).getNecessaryElectiveCourses() == 0 && {
                //(**it_student).getCurrentSemester() == this->_semesters) {

            // Log to file.
            writeToStudentsLogFile(
                    (**it_student).getId(), "", "", GRADUATED); 

            if ((**it_student).getDepartment().compare(_CS_) == 0) {
                saveColorImage(
                        csGraduationImage,
                        **it_student,
                        numberOfCsImages);
            } else {  // PG student.
                saveColorImage(
                        pgGraduationImage,
                        **it_student,
                        numberOfPgImages);
            }
        } else {

            // Log to file if CS or (PG & Malag is on).
            if ((**it_student).getDepartment().compare(_CS_) == 0 ||
                    this->_pgOn) {

                writeToStudentsLogFile(
                        (**it_student).getId(), "", "", NOT_GRADUATED);
            }

            if ((**it_student).getDepartment().compare(_CS_) == 0) {
                saveGreyscaleImage(
                        csGraduationImage,
                        **it_student,
                        numberOfCsImages);
            } else {  // PG student.
                saveGreyscaleImage(
                        pgGraduationImage,
                        **it_student,
                        numberOfPgImages);
            }
        }
    }

    csGraduationImage.displayImage(_CS_);
    pgGraduationImage.displayImage(_PG_);

    csGraduationImage.saveImage(CS_IMAGE_FILE);
    pgGraduationImage.saveImage(PG_IMAGE_FILE);
}


bool Uni :: isStudentInCourse(Course &course, Student &student) const {

    vector<Student *>::iterator it_student;
    for (it_student = course.getStudents().begin();
            it_student != course.getStudents().end(); ++it_student) {

        if ((**it_student).getId().compare(student.getId()) == 0) {
            return true;
        }
    }

    return false;
}


void Uni :: registerStudentToMandatoryCourses(
        vector<Course *> &mandatorySemesterCourses,
        Student &student) {

    string department = student.getDepartment();
    unsigned short semester = student.getCurrentSemester();

    vector<Course *>::iterator it_mandatoryCourse;

    // If student passed last semester successfully,
    // register student to his department courses this semester,
    for (it_mandatoryCourse = mandatorySemesterCourses.begin();
            it_mandatoryCourse != mandatorySemesterCourses.end();
            ++it_mandatoryCourse) {

        if ((**it_mandatoryCourse).getDepartment().compare(department) == 0 &&
                ((**it_mandatoryCourse).getSemester() == semester +1)) {

            (**it_mandatoryCourse).reg(student);
        }
    }
}


void Uni :: registerStudentToElectiveCourses(
        vector<Course *> &electiveSemesterCourses,
        Student &student) {

    vector<Course *>::iterator it_electiveCourse;

    // Register student to minimum number of elective courses necessary to graduate.
    for (it_electiveCourse = electiveSemesterCourses.begin();
            it_electiveCourse != electiveSemesterCourses.end();
            ++it_electiveCourse) {

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

        if ((**it_student).getUnfinishedSemesterMandatoryCourses() == 0 &&
                (**it_student).getUnfinishedSemesterElectiveCourses() == 0) {

            (**it_student).promoteToNextSemster();
        }
    }
}


void Uni :: saveColorImage(
        ImageLoader &image,
        Student& student,
        unsigned int &numberOfImages) {
    ImageOperations opr;

    string studentImagePath = student.getImagePath();
    studentImagePath.erase(
            studentImagePath.find_last_not_of(" \n\r\t") +1);

    ImageLoader studentImage(studentImagePath);
    ImageLoader resizedStudentImage(
            PROFILE_IMAGE_SIZE, PROFILE_IMAGE_SIZE);

    opr.resize(studentImage.getImage(), resizedStudentImage.getImage());

    opr.copy_paste_image(
            resizedStudentImage.getImage(),
            image.getImage(),
            numberOfImages * PROFILE_IMAGE_SIZE);

    numberOfImages++;
}


void Uni :: saveGreyscaleImage(
        ImageLoader &image,
        Student &student,
        unsigned int &numberOfImages) {
    ImageOperations opr;

    string studentImagePath = student.getImagePath();
    studentImagePath.erase(studentImagePath.find_last_not_of(" \n\r\t") +1);

    ImageLoader studentImage(studentImagePath);
    ImageLoader resizedStudentImage(PROFILE_IMAGE_SIZE, PROFILE_IMAGE_SIZE);

    opr.resize(studentImage.getImage(), resizedStudentImage.getImage());

    opr.rgb_to_greyscale(
            resizedStudentImage.getImage(),
            resizedStudentImage.getImage());

    opr.copy_paste_image(
            resizedStudentImage.getImage(),
            image.getImage(),
            numberOfImages * PROFILE_IMAGE_SIZE);

    numberOfImages++;
}


void Uni :: deleteCourses(vector<Course *> &courses) {

    vector<Course *>::iterator it_course;

    for (it_course = courses.begin();
            it_course != courses.end();
            ++it_course) {

        delete *it_course;
        *it_course = 0;
    }
}
