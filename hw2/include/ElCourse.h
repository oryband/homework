#ifndef ELCOURSE_H_
#define ELCOURSE_H_


#include "Student.h"
#include "Course.h"

#include "util.h"


class ElCourse : public Course {

    public:
        ElCourse();
        ElCourse(
                std::string name,
                unsigned short semester,
                unsigned short minimumGrade); 

        void reg(Student &s);
       ~ElCourse();
};

#endif
