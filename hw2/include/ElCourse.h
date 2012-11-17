#ifndef ELCOURSE_H_
#define ELCOURSE_H_

class ElCourse : public Course{ // fix the constructor

public:
		ElCourse( string courseName,unsigned short semesterNum,
								   unsigned short minimumGrade );
		virtual void reg(Student &s);

};

#endif
