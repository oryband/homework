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
                std::string imagePath,
                unsigned short electiveCourses,
                std::string department);

        inline virtual ~Student() {}

        virtual void study(Course &course)=0;
        void finishcourse(Course &course);

        // Getters.
        const std::string getId() const;
        const std::string getDepartment() const;
        const std::string getImagePath() const; 
        unsigned short getCurrentSemester() const;
        unsigned short getUnfinishedSemesterMandatoryCourses() const;
        unsigned short getUnfinishedSemesterElectiveCourses() const;
        unsigned short getNecessaryElectiveCourses() const;

        // Setters
        void incrementUnfinishedSemesterMandatoryCourses(); 
        void incrementUnfinishedSemesterElectiveCourses(); 
        void promoteToNextSemster();
};


struct compareStudents {
    bool operator() (Student const *s1, Student const *s2) const {
        unsigned int id1 = atoi(s1->getId().c_str()),
                     id2 = atoi(s2->getId().c_str());

        return id1 < id2;
    }
};

#endif
