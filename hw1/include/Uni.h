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
        void printAssignment(
                std::string coursesOutputPath,
                std::string studentsOutputPath);
    private:
        std::vector<Course> courses;
        std::vector<Student> unassignedStudents;

        void readCourses(std::vector< std::vector<std::string> >* lines);
        void readStudents(std::vector< std::vector<std::string> >* lines);

        void printCoursesToFile(std::string coursesOutputPath);
        void printStudentsToFile(std::string studentsOutputPath);
    
        void getLines(
                std::string filePath,
                std::vector< std::vector<std::string> >* lines);
};

#endif
