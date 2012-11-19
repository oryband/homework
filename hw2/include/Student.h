#ifndef STUDENT_H_
#define STUDENT_H_

#include "Course.h"

#include "typedefs.h"
#include "consts.h"

#include <iostream>
#include <string>
#include <cstdlib>
#include <vector>


class Student {

    protected:
        std::string _id;
        std::string _department;
        std::string _imagePath;

        unsigned short _unfinishedSemesterCourses;
        unsigned short _unfinishedElectiveCourses;

        unsigned short _currentSemester;

    public:
        Student(std::string id,
                std::string department,
                std::string imagePath,
                unsigned short electiveCourses);

        virtual ~Student();

        virtual void study(Course &course)=0;  // TODO
        void finishcourse(Course &course);  // TODO

        // Getters.
        std::string getStudentId();
        unsigned short getUnfinishedSemesterCourses();
        unsigned short getUnfinishedElectiveCourses();
        unsigned short getCurrentSemester();

        // Setters
        // _unfinishedSemesterCourses++/--;
        void increaseUnfinishedSemesterCourses();
        void decreaseUnfinishedElectiveCourses();
};

#endif
