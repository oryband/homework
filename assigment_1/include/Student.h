#ifndef STUDENT_H
#define STUDENT_H

#include <string>
#include <vector>


class Student {
    public:
        string name;
        vector<string> courses;
    
        Student(string newName, vector<string> *newCourses) {
            name = string(newName);
            courses = *newCourses;
        }
};

#endif
