#ifndef UNI_H
#define UNI_H

#include "Course.h"
#include "Student.h"

#include <string>
#include <vector>
#include <iostream>
#include <fstream>
#include <sstream>
#include <iomanip>
#include <cstdlib>


#define DAYS 7


class Uni {
    public:
        Uni(std::string coursesPath, std::string studentsPath);

        void assignStudents();
        void printAssignment();
    private:
        std::vector<Course> courses;
        std::vector<Student> unassignedStudents;

        void readCoursesFile(std::string coursesPath);
        void readStudentsFile(std::string studentsPath);

        std::vector< std::vector<std::string> >*
            getLines(std::string filePath);
};

#endif
