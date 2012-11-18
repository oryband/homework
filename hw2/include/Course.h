#ifndef COURSE_H_
#define COURSE_H_

#include <iostream>
#include <vector>
#include <iterator>
#include <string>


class Student;


class Course {

    protected:
        std::vector< Student > students;  // TODO: Shared pointer
        std::string _name;  // Delete const ...doing some crazy  problems

        unsigned short _semester;
        unsigned short _minimumGrade;

    public:
        virtual void teach();
        virtual void reg(Student &s)=0;
        unsigned short getMinGrade()const;

        virtual ~Course();
};

#endif
