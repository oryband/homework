#ifndef UNI_H_
#define UNI_H_


#include <algorithm>

#include "Student.h"
#include "CsStudent.h"
#include "PgStudent.h"

#include "Course.h"
#include "CsCourse.h"
#include "PgCourse.h"
#include "ElCourse.h"

#include "typedefs.h"
#include "consts.h"
#include "util.h"


class Uni {
    private:
        unsigned short _semesters;
        bool _pgOn;  // Has MALAG approved PG courses?

        unsigned short _CsNumOfElctiveCourses;
        unsigned short _PgNumOfElctiveCourses;

        std::vector<Student *> _students;

        // Note we have to use <Course *> because Course is ABSTRACT.
        // TODO: Do we need to delete inner members in dtor?
        std::vector<Course *> _mandatoryAutumnCourses;
        std::vector<Course *> _mandatorySpringCourses;

        std::vector<Course *> _electiveAutumnCourses;
        std::vector<Course *> _electiveSpringCourses;

        void registerStudentToMandatoryCourses(
                std::vector<Course *> &mandatorySemesterCourses,
                Student &student);

        void registerStudentToElectiveCourses(
                std::vector<Course *> &electiveSemesterCourses,
                Student &student);

        const bool isStudentInCourse(Course &course, Student &student) const;

    public:
        Uni(bool pgOn);
        ~Uni();  // TODO

        void readStudentsFile(
                unsigned short CsNumOfElc,
                unsigned short PgNumOfElc);

        void readCurriculumFile();
        void readCoursesFile();

        void simulate();

        void registerStudentsToCourses(unsigned short currentSemester);
        void teach(unsigned short currentSemester);
        void promoteStudents();
        void graduate();
        void generateGraduationImage(vector<Student *> &students);

        void SaveColorImage(Student &student);  // TODO
        void SaveGreyscaleImage(Student &student);  // TODO

        void deleteVectorCourses(vector<Course *>* vectorCourses);
};

#endif
