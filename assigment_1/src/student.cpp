#include <iostream>
#include <string.h>
#include <vector>
//#include <cstring>
#include <string>

using namespace std;

class Student{
   
    public:
        string name;
        vector<string> inCourses;
    
        Student(string newName, vector<string> *Courses) {
            name = string(newName);
            inCourses = *Courses;
        }
};

