#ifndef COURSE_H_
#define COURSE_H_

#include <iostream>
#include <vector>
#include <iterator>
#include <string>

#include "Student.h"
class Student;


class Course {

protected:
		std::vector< Student > students;
		const std::string _courseName;
		unsigned short _semester;
		unsigned short _minimumGrade;

		Course(string courseName,unsigned short semesterNum,
								 unsigned short minimumGrad);
public:
		virtual void teach();
		virtual void reg(Student &s)=0;
		unsigned short getMinGrade()const;
		virtual ~Course(){ std::cout << "Now the Course is dead!" << std::endl; }

};
#endif
