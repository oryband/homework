#ifndef STUDENT_H
#define STUDENT_H

#include <string>
#include <vector>


class Student {
    public:
        std::string name;
        std::vector<std::string> courses;
        std::vector<unsigned short> weekdays;  // Corresponding courses' weekdays.

        Student(std::string sname, std::vector<std::string>* scourses);
};

#endif
