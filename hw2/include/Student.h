#ifndef STUDEND_H_
#define STUDEND_H_

#include <iostream>
#include <string>


class Course;


class Student {

protected:
		std::string _id;
		std::string _imagePath;

		unsigned short _unfinishedSemesterCourses;
		unsigned short _unfinishedElectiveCourses;

		unsigned short _currentSemester;

public:
		void finishcourse(Course &c);
		virtual void study(Course &c)=0;
		virtual ~Student() { std::cout << "Student is dead!" << std::endl;}

        std::string getStudentId();
        unsigned short getUnfinishedSemesterCourses();
        unsigned short getUnfinishedElectiveCourses();
};

#endif
