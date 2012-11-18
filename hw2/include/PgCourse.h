#ifndef PGCOURSE_H_
#define PGCOURSE_H_

class PgCourse : public Course{

public:
	PgCourse(std::string courseName,unsigned short semesterNum,
									   unsigned short minimumGrade );
		virtual void reg(Student &s);
};

#endif
