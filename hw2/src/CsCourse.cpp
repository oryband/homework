#include "CsCourse.h"


//  Huge issue with logic..need to clear it out!!!
void CsCourse :: reg(StudentPointer &s) {

    this->pushToCourse(s);
    writeToFileStudents(s->getStudentId(), this->_name, CS, 1);

}
