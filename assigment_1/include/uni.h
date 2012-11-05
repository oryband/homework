#ifndef UNI_H
#define UNI_H

#include <string>


class Uni {
    private:
        vector<Course> courses;
        vector<Student> unassignedStudents;

        vector<Course>* readCourses();
        vector<Student>* readStudents();

    public:
        Uni(string coursesPath, string studentsPath);
        ~Uni();

        void assignStudents();
        void printAssignment();
};

#endif
