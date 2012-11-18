#ifndef UNI_H_
#define UNI_H_

#include "Student.h"
#include "CsStudent.h"
#include "PgStudent"
#include "Course.h"

#include "consts.cpp"
#include "util.cpp"


class Uni {
    private:
        unsigned short _semesters;
        bool _pgOn;  // Malag approved PG courses.

        unsigned short _CsNumOfElctiveCourses;
        unsigned short _PgNumOfElctiveCourses;

        std::vector<Student> _students;  // TODO: Shared pointer.

        std::vector<Course> _mandatoryAutumnCourses;
        std::vector<Course> _mandatorySpringCourses;

        std::vector<Course> _electiveAutumnCourses;
        std::vector<Course> _electiveSpringCourses;

        void registerStudentToMandatoryCourses(
                std::vector<Course> &mandatorySemesterCourses,
                Student &student);

        void registerStudentToElectiveCourses(
                std::vector<Course> &electiveSemesterCourses,
                Student &student);

        bool studentInCourse(Course &course, Student &student);

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
