#ifndef COURSES_H
#define COURSES_H

#include "Student.h"

#include <vector>


class Course {
    public:
        Course(unsigned short cweekday, std::string cid, unsigned short cspace) {
            weekday = cweekday;
            id = cid;
            space = cspace;
        }

        std::string id;
        unsigned short space;
        unsigned short weekday;
        std::vector<Student> assignedStudents;
};

#endif
