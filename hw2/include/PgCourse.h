#ifndef PGCOURSE_H_
#define PGCOURSE_H_


#include "Course.h"

#include "util.h"
#include "consts.h"


class Student;


class PgCourse : public Course {
    public:
        PgCourse(
                std::string name,
                unsigned short semester,
                unsigned short minimumGrade) :
            Course(name, PG, semester, minimumGrade) {}

        void reg(Student &s);
};

#endif
