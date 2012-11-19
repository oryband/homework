#ifndef PGCOURSE_H_
#define PGCOURSE_H_


#include "Course.h"

#include "typedefs.h"


class PgCourse : public Course {
    public:
        PgCourse(
                std::string name,
                unsigned short semester,
                unsigned short minimumGrade) :
            Course(name, PG, semester, minimumGrade) {}

        virtual ~PgCourse();

        virtual void reg(Student &student);
};

#endif
