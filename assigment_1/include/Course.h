#ifndef COURSES_H
#define COURSES_H

#include "Student.h"


class Course {
    public:
        Course(unsigned short cweekday, std::string cid, unsigned short cspace);

        std::string id;
        unsigned short space;
        unsigned short weekday;
        std::vector<Student> assignedStudents;
};

#endif
