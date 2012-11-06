#include <iostream>
#include <fstream>
#include <string>
//#include <vector>

#include "Uni.h"

using namespace std;


Uni::Uni(string coursesPath, string studentsPath) {

        fstream fileCourses;

        fileCourses.open(coursesPath);

        if(coursesPath.is_open()){

        




        }
        else{
        cout << "Unable to open file courses.conf!!"<<endl;
        }

}

/*void Uni::assignStudents() {
    vector<Student>::iterator c;
    for (s = this.students.begin(); s != this.studentsPath.end(); ++s) {
    }
}*/
