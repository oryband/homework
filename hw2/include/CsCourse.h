#ifndef CSCOURSE_H_
#define CSCOURSE_H_

class CsCourse : public Course{ // fix the constructor

public:
		CsCourse(std::string courseName,unsigned short semesterNum,
								   unsigned short minimumGrade );
		virtual void reg(Student &s);

};

#endif
