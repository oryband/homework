#ifndef ELCOURSE_H_
#define ELCOURSE_H_


#include "Course.h"

#include "typedefs.h"


class ElCourse : public Course {

    public:
        ElCourse(
                std::string name,
                unsigned short semester,
                unsigned short minimumGrade) :
            Course(name, ELECTIVE, semester, minimumGrade) {}

        virtual ~ElCourse();

        virtual void reg(Student &student);
};

#endif
