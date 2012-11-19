#ifndef PGCOURSE_H_
#define PGCOURSE_H_


#include "Course.h"


class PgCourse : public Course {
    public:
        PgCourse(
                std::string name,
                unsigned short semester,
                unsigned short minimumGrade) :
            Course(name, semester, minimumGrade) {}

        virtual void reg(StudentPointer &s);
};

#endif
