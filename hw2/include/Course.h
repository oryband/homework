#ifndef COURSE_H_
#define COURSE_H_


#include <string>
#include <vector>

#include "consts.h"


class Student;


class Course {
    protected:
        std::string _name;
        std::string _department;
        unsigned short _semester;
        unsigned short _minimumGrade;

        std::vector<Student *> _students;

        void pushToCourse(Student *ptr_student);

    public:
        Course( std::string name,
                std::string department,
                unsigned short semester,
                unsigned short minimumGrade);

        inline virtual ~Course() {}

        virtual void reg(Student &s)=0;
        virtual void teach();

        // Getters
        const unsigned short getMinimumGrade() const;
        std::vector<Student *> &getStudents();
        const std::string getName() const;
        const std::string getDepartment() const;
};

#endif
