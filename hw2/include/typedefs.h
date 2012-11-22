#ifndef TYPEDEFS_H_
#define TYPEDEFS_H_


#include <boost/function.hpp>

#include "Student.h"


bool compareStudents (Student *s1, Student *s2) {
    return (
            atoi(s1->getStudentId().c_str()) < 
            atoi(s2->getStudentId().c_str()));
}


boost::function<bool (Student *s1, Student *s2)>
    CompareStudentsFunctor(&compareStudents);


#endif
