#ifndef ELCOURSE_H_
#define ELCOURSE_H_


#include "Course.h"
#include "Student.h"

#include "util.h"


class ElCourse : public Course {

    public:
        ElCourse(
                std::string name,
                unsigned short semester,
                unsigned short minimumGrade) :
            Course(name, ELECTIVE, semester, minimumGrade) {}

        ~ElCourse();

        void reg(Student &s);
};

#endif
