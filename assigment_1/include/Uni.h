#ifndef UNI_H
#define UNI_H

#include "Course.h"
#include "Student.h"

#include <string>


class Uni {
    public:
        Uni(string coursesPath, string studentsPath);
        ~Uni();

        void assignStudents();
        void printAssignment();
    private:
        vector<Course> courses;
        vector<Student> unassignedStudents;

        vector<Course>* readCourses();
        vector<Student>* readStudents();

};

#endif
