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
#include "ImageLoader.h"
#include "ImageOperations.h"

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
        unsigned short _numOfCsStudents;
        unsigned short _numOfPgStudents;
        
        unsigned short _numOfCsStuInImage;
        unsigned short _numOfPgStuInImage;

        // Note we have to use <Course *> because Course is ABSTRACT.
        // TODO: Do we need to delete inner members in dtor?
        std::vector<Course *> _mandatoryAutumnCourses;
        std::vector<Course *> _mandatorySpringCourses;

        std::vector<Course *> _electiveAutumnCourses;
        std::vector<Course *> _electiveSpringCourses;

        //  Pictures CS,PG
        ImageLoader _csPicture;
        ImageLoader _pgPicture;

        void registerStudentToMandatoryCourses(
                std::vector<Course *> &mandatorySemesterCourses,
                Student &student);

        void registerStudentToElectiveCourses(
                std::vector<Course *> &electiveSemesterCourses,
                Student &student);

        const bool isStudentInCourse(Course &course, Student &student) const;

        void deleteCourses(std::vector<Course *> &courses);

    public:
        Uni(bool pgOn);
        ~Uni();

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

        void saveColorImage(Student &student);
        void saveGreyscaleImage(Student &student);

        void deleteVectorCourses(vector<Course *>* vectorCourses);

};

#endif
