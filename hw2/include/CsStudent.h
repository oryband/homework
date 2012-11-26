#ifndef CSSTUDENT_H_
#define CSSTUDENT_H_


#include <string>
#include <cstdlib>
#include <time.h>

#include "Student.h"

#include "consts.h"
#include "util.h"


class Course;


class CsStudent : public Student {
    public:
        CsStudent(
                std::string studentId,
                std::string imagePath,
                unsigned short electiveCourses) :
            Student(studentId, imagePath, electiveCourses, _CS_) {}

        void study(Course &course);
};

#endif
