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

        // FIXME: No descrutor needed, right? Please check.

        void assignStudents();
        void printAssignment(
                std::string coursesOutputPath,
                std::string studentsOutputPath);
    private:
        std::vector<Course> courses;
        std::vector<Student> unassignedStudents;

        void readCoursesFile(std::string coursesPath);
        void readStudentsFile(std::string studentsPath);

        void printCoursesToFile(std::string coursesOutputPath);
        void printStudentsToFile(std::string studentsOutputPath);

        std::vector< std::vector<std::string> >*
            getLines(std::string filePath);
};

#endif
