#ifndef STUDENT_H
#define STUDENT_H

#include <string>
#include <vector>


class Student {
    public:
        std::string name;
        std::vector<std::string>* courses;
    
        Student(std::string sname, std::vector<std::string>* scourses);
        ~Student();
};

#endif
