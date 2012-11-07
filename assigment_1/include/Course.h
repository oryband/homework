#ifndef COURSES_H
#define COURSES_H

#include "Student.h"


class Course {
    public:
        Course(std::string cid, unsigned short cspace);
        ~Course();

        std::string id;
        unsigned short space;
        std::vector<Student>* assignedStudents;
};

#endif
