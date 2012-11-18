#ifndef COURSE_H_
#define COURSE_H_


#include "typedefs.h"

#include <vector>
#include <string>


class Student;


class Course {
    protected:
        std::vector<StudentPointer> _students;
        std::string _name;

        unsigned short _semester;
        unsigned short _minimumGrade;

    public:
        virtual void teach();
        virtual void reg(StudentPointer &s)=0;

        unsigned short getMinimumGrade() const;
        std::vector<StudentPointer>* getStudents();

        virtual ~Course();
};

#endif
