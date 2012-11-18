#ifndef ELCOURSE_H_
#define ELCOURSE_H_


#include "Course.h"


class ElCourse : public Course {

    public:
        ElCourse(
                std::string courseName,
                unsigned short semesterNum,
                unsigned short minimumGrade);

        virtual void reg(Student &s);
};

#endif
