#ifndef STUDEND_H_
#define STUDEND_H_

#include <iostream>
#include <string>

class Course;

class Student{

protected:
		std::string _studentId; // rm const
		std::string _imagePath; // rm const
		unsigned short _unfinishedSemesterCourses;
		unsigned short _currentSemester;
		unsigned short _electiveCoursesUnfinished;

		Student(){};

public:
		void finishcourse(Course &c);
		virtual void study(Course &c)=0;
		virtual ~Student() { std::cout << "Student is dead!" << std::endl;}

};

#endif
