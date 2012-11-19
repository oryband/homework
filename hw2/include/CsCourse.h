#ifndef CSCOURSE_H_
#define CSCOURSE_H_


#include "Course.h"
#include "typedefs.h"
#include "consts.h"


class CsCourse : public Course {   

    public:
        CsCourse(
                std::string name,
                unsigned short semester,
                unsigned short minimumGrade) :
            Course(name, semester, minimumGrade) {}

        virtual void reg(StudentPointer &s);
};

#endif
