#ifndef UNI_H_
#define UNI_H_


#include <algorithm>
#include <sstream>

#include "Student.h"
#include "CsStudent.h"
#include "PgStudent.h"

#include "Course.h"
#include "CsCourse.h"
#include "PgCourse.h"
#include "ElCourse.h"

//#include "ImageLoader.h"
//#include "ImageOperations.h"

#include "consts.h"
#include "util.h"


class Uni {
    private:
        std::vector<Student *> _students;

        std::vector<Course *> _mandatoryAutumnCourses;
        std::vector<Course *> _mandatorySpringCourses;

        std::vector<Course *> _electiveAutumnCourses;
        std::vector<Course *> _electiveSpringCourses;

        unsigned short _semesters;
        bool _pgOn;  // Has MALAG approved PG courses?

        unsigned short _numOfCsStudents;
        unsigned short _numOfPgStudents;

        unsigned short _numOfCsStuInImage;
        unsigned short _numOfPgStuInImage;

        void readCurriculumFile(
                unsigned short &numOfCsElectiveCourses,
                unsigned short &numOfPgElectiveCourses);

        void readStudentsFile(
                unsigned short &csElectiveCourses,
                unsigned short &pgElectiveCourses);

        void readCoursesFile();

        void registerStudentToMandatoryCourses(
                std::vector<Course *> &mandatorySemesterCourses,
                Student &student);

        void registerStudentToElectiveCourses(
                std::vector<Course *> &electiveSemesterCourses,
                Student &student);

        const bool isStudentInCourse(
                Course &course, Student &student) const;

        /*void saveColorImage(ImageLoader &image, Student &student);
        void saveGreyscaleImage(ImageLoader &image, Student &student);*/

        void deleteCourses(std::vector<Course *> &courses);

    public:
        Uni(bool pgOn);
        ~Uni();

        void simulate();

        void registerStudentsToCourses(unsigned short currentSemester);
        void teach(unsigned short currentSemester);
        void promoteStudents();
        void graduate();
};
#endif
