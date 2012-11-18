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

#endif
