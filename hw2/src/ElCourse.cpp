#include "ElCourse.h"

ElCourse :: ElCourse() : Course("","",2,2)
{

}

ElCourse :: ElCourse(
                std::string name,
                unsigned short semester,
                unsigned short minimumGrade) :
            Course(name, "ELECTIVE", semester, minimumGrade){} 

void ElCourse :: reg(Student &s) {
    this->pushToCourse(&s);

    Util::writeToStudentsLogFile(
            s.getStudentId(),
            this->_name,
            ELECTIVE,
            TAKING_COURSE);
}

ElCourse :: ~ElCourse() { cout << "ElCourse dead" << endl;}
