#ifndef STUDENT_H_
#define STUDENT_H_


#include <string>
#include <vector>
#include <cstdlib>
#include <iostream>

#include "typedefs.h"
#include "consts.h"


class Course;


class Student {

    protected:
        std::string _id;
        std::string _department;
        std::string _imagePath;

        unsigned short _unfinishedSemesterMandatoryCourses;
        unsigned short _unfinishedSemesterElectiveCourses;

        unsigned short _necessaryElectiveCourses;

        unsigned short _currentSemester;

    public:
        Student(std::string id,
                std::string department,
                std::string imagePath,
                unsigned short electiveCourses);

        virtual ~Student();

        virtual void study(Course &course)=0;
        void finishcourse(Course &course);

        // Getters.
        std::string getStudentId();
        const unsigned short getUnfinishedSemesterMandatoryCourses() const;
        const unsigned short getUnfinishedSemesterElectiveCourses() const;
        const unsigned short getNecessaryElectiveCourses() const;
        const unsigned short getCurrentSemester() const;
        const std::string getDepartmentName() const;
        // Setters
        void incrementUnfinishedSemesterMandatoryCourses(); 
        void incrementUnfinishedSemesterElectiveCourses(); 
        void promoteToNextSemster();
};

#endif
