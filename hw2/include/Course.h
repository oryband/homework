#ifndef COURSE_H_
#define COURSE_H_


#include <string>
#include <vector>

#include "typedefs.h"


class Student;


class Course {

    protected:
        std::string _name;
        unsigned short _semester;
        unsigned short _minimumGrade;

        std::vector<StudentPointer> _students;

    public:
        Course(
                std::string name,
                unsigned short semester,
                unsigned short minimumGrade);

        virtual ~Course();

        virtual void teach();
        virtual void reg(StudentPointer &s)=0;

        // Getters
        unsigned short getMinimumGrade() const;
        std::vector<StudentPointer>* getStudents();
};

#endif
