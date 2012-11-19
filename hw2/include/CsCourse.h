#ifndef CSCOURSE_H_
#define CSCOURSE_H_


#include "Course.h"

#include "typedefs.h"
#include "consts.h"


class CsCourse : public Course {

    public:
        CsCourse() : Course("", CS, 0, 0) {}
        CsCourse(
                std::string name,
                unsigned short semester,
                unsigned short minimumGrade) :
            Course(name, CS, semester, minimumGrade) {}

        virtual ~CsCourse();

        virtual void teach();
        virtual void reg(Student &student);
        virtual void teachStudent(Student &student);
};

#endif
