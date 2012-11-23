#ifndef CSCOURSE_H_
#define CSCOURSE_H_


#include "Course.h"

#include "util.h"
#include "consts.h"


class Student;


class CsCourse : public Course {
    public:
        CsCourse(
                std::string name,
                unsigned short semester,
                unsigned short minimumGrade) :
            Course(name, CS, semester, minimumGrade) {}

        void reg(Student &s);
};

#endif
