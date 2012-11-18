#ifndef PGSTUDENT_H_
#define PGSTUDENT_H_

#include "Student.h"

class PgStudent : public Student {

public:
		PgStudent(std::string studentId, std::string imagePath);
		virtual void study(Course &c);
};

#endif
