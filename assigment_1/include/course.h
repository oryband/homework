#ifndef COURSES_H
#define COURSES_H

class Course {
    public:
        unsigned short id;
        unsigned short space;
        unsigned short weekday;
        vector<Student> assignedStudents;
};

#endif
