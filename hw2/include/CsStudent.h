#ifndef CSSTUDENT_H_
#define CSSTUDENT_H_

#include "Student.h"

class CsStudent : public Student {

public:
		CsStudent(std::string studentId, std::string imagePath);
		virtual void study(Course &c);
};

#endif
