#ifndef CSSTUDENT_H_
#define CSSTUDENT_H_


#include "Student.h"
#include "Course.h"

class CsStudent : public Student {

    public:
        CsStudent(
                std::string studentId,
                std::string imagePath,
                unsigned short electiveCourses) :
            Student(studentId, CS, imagePath, electiveCourses) {}

        virtual void study(Course &c);
};

#endif
