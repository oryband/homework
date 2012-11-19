#ifndef COURSE_H_
#define COURSE_H_


#include <string>
#include <vector>

#include "typedefs.h"
#include "util.h"


class Course {
    protected:
        std::string _name;
        std::string _department;
        unsigned short _semester;
        unsigned short _minimumGrade;

        std::vector<Student *> _students;

        void pushToCourse(Student *ptr_student);

    public:
        Course(
                std::string name,
                std::string department,
                unsigned short semester,
                unsigned short minimumGrade);

        virtual ~Course();

        virtual void teach();
        virtual void teachStudent(Student &student)=0;
        virtual void reg(Student &student)=0;

        // Getters
        // TODO Should we add consts at return value (beginning of each line) ?
        unsigned short getMinimumGrade() const;
        // TODO Perhaps we need to remove const for Student.finishCourse() to work?
        const std::vector<Student *>& getStudents();
        string getCourseName() const;
        string getCourseDepartment()const;
};

#endif
