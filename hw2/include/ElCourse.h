#ifndef ELCOURSE_H_
#define ELCOURSE_H_


#include "Course.h"


class ElCourse : public Course {

    public:
        ElCourse(
                std::string name,
                unsigned short semester,
                unsigned short minimumGrade) :
            Course(name, semester, minimumGrade) {}

        virtual void reg(Student &s);
};

#endif
