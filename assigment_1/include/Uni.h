#ifndef UNI_H
#define UNI_H

#include "Course.h"
#include "Student.h"

#include <string>
#include <vector>
#include <iostream>
#include <fstream>


class Uni {
    public:
        Uni(std::string coursesPath, std::string studentsPath);
        ~Uni();

        void assignStudents();
        void printAssignment();
    private:
        std::vector<Course> courses;
        std::vector<Student> unassignedStudents;

        std::vector<Course>* readCourses();
        std::vector<Student>* readStudents();

};

#endif
