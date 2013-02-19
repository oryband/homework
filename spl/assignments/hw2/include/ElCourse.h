#ifndef ELCOURSE_H_
#define ELCOURSE_H_


#include "Course.h"

#include "util.h"
#include "consts.h"


class Student;


class ElCourse : public Course {
    public:
        ElCourse(
                std::string name,
                unsigned short semester,
                unsigned short minimumGrade) :
            Course(name, _ELECTIVE_, semester, minimumGrade) {}

        void reg(Student &s);
};

#endif
