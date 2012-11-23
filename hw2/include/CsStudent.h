#ifndef CSSTUDENT_H_
#define CSSTUDENT_H_


#include <string>

#include "Student.h"

#include "util.h"


class Course;


class CsStudent : public Student {
    public:
        CsStudent(
                std::string studentId,
                std::string imagePath,
                unsigned short electiveCourses) :
            Student(studentId, CS, imagePath, electiveCourses) {}

        void study(Course &course);
};

#endif
