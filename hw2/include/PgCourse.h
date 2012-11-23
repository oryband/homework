#ifndef PGCOURSE_H_
#define PGCOURSE_H_


#include "Course.h"
#include "Student.h"

#include "util.h"


class PgCourse : public Course {
    public:
        PgCourse(
                std::string name,
                unsigned short semester,
                unsigned short minimumGrade) :
            Course(name, PG, semester, minimumGrade) {}

        void reg(Student &s);
        ~PgCourse();
};

#endif
