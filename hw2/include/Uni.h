#ifndef UNI_H_
#define UNI_H_


#include "Student.h"
#include "CsStudent.h"
#include "PgStudent.h"
#include "Course.h"

#include "util.h"
#include "typedefs.h"
#include "consts.h"

//#include <string>


class Uni {
    private:
        unsigned short _semesters;
        bool _pgOn;  // Has MALAG approved PG courses?

        unsigned short _CsNumOfElctiveCourses;
        unsigned short _PgNumOfElctiveCourses;

        std::vector<StudentPointer> _students;

        std::vector<Course> _mandatoryAutumnCourses;
        std::vector<Course> _mandatorySpringCourses;

        std::vector<Course> _electiveAutumnCourses;
        std::vector<Course> _electiveSpringCourses;

        void registerStudentToMandatoryCourses(
                std::vector<Course> &mandatorySemesterCourses,
                StudentPointer &student);

        void registerStudentToElectiveCourses(
                std::vector<Course> &electiveSemesterCourses,
                StudentPointer &student);

        bool isStudentInCourse(Course &course, StudentPointer &student);

    public:
        Uni(bool pgOn);

        void simulate();
        void registerStudentsToCourses(unsigned short currentSemester);
        void teach(unsigned short currentSemester);
        void gruduate();

        void readStudentsFile(
                unsigned short CsNumOfElc,
                unsigned short PgNumOfElc);

        void readCurriculumFile();
        void readCoursesFile();
};

#endif
