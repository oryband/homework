#ifndef TYPEDEFS_H_
#define TYPEDEFS_H_


#include "Student.h"
#include "CsStudent.h"
#include "PgStudent.h"
//#include "Course.h"

#include <boost/shared_ptr.hpp>


typedef boost::shared_ptr<Student> StudentPointer;
typedef boost::shared_ptr<CsStudent> CsStudentPointer;
typedef boost::shared_ptr<PgStudent> PgStudentPointer;
//typedef boost::shared_ptr<Course> CoursePointer;


bool compareStudents (
        const StudentPointer &s1,
        const StudentPointer &s2) {

    return (
            atoi(s1->getStudentId().c_str()) < 
            atoi(s2->getStudentId().c_str()));
}


#endif
