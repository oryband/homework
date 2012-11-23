#ifndef PGSTUDENT_H_
#define PGSTUDENT_H_


#include <string>

#include "Student.h"

#include "util.h"
#include "consts.h"


class Course;


class PgStudent : public Student {
    public:
        PgStudent(
                std::string studentId,
                std::string imagePath,
                unsigned short electiveCourses) :
            Student(studentId, PG, imagePath, electiveCourses) {}

        void study(Course &course);
};

#endif
