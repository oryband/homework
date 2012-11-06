#ifndef STUDENT_H
#define STUDENT_H

#include <string>
#include <vector>


class Student {
    public:
        std::string name;
        std::vector<std::string> courses;
    
        Student(std::string newName, std::vector<std::string> *newCourses) {
            name = std::string(newName);
            courses = *newCourses;
        }
};

#endif
