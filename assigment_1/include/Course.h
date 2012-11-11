#ifndef COURSES_H
#define COURSES_H

#include "Student.h"


class Course {
    public:
        Course(unsigned short cweekday,
                std::string cid,
                unsigned short cspace):
            id(cid),
            weekday(cweekday),
            space(cspace),
            assignedStudents() {}

        std::string id;
        unsigned short weekday;
        unsigned short space;
        std::vector<Student> assignedStudents;
};

#endif
