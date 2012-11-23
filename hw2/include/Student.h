#ifndef STUDENT_H_
#define STUDENT_H_


#include <string>
#include <vector>
#include <cstdlib>
#include <iostream>

#include "consts.h"


class Course;


class Student {

    protected:
        std::string _id;
        std::string _department;
        std::string _imagePath;

        unsigned short _currentSemester;

        unsigned short _unfinishedSemesterMandatoryCourses;
        unsigned short _unfinishedSemesterElectiveCourses;

        unsigned short _necessaryElectiveCourses;

    public:
        Student(std::string id,
                std::string department,
                std::string imagePath,
                unsigned short electiveCourses);

        inline virtual ~Student() {}

        virtual void study(Course &course)=0;
        void finishcourse(Course &course);

        // Getters.
        const std::string getStudentId() const;
        const std::string getDepartment() const;
        const std::string getImagePath() const; 
        const unsigned short getCurrentSemester() const;
        const unsigned short getUnfinishedSemesterMandatoryCourses() const;
        const unsigned short getUnfinishedSemesterElectiveCourses() const;
        const unsigned short getNecessaryElectiveCourses() const;

        // Setters
        void incrementUnfinishedSemesterMandatoryCourses(); 
        void incrementUnfinishedSemesterElectiveCourses(); 
        void promoteToNextSemster();
};


struct compareStudents {
    bool operator()(Student const *s1, Student const *s2) const {
        return s1->getStudentId().c_str() < s2->getStudentId().c_str();
    }
};

#endif
