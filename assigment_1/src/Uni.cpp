#include "Uni.h"

using namespace std;


Uni::Uni(string coursesPath, string studentsPath) {
}

/* Iterate over each student's applied courses,
 * and assign them to available courses.
 */
void Uni::assignStudents() {
    vector<Student>::iterator student;
    vector<Course>::iterator course;
    vector<string>::iterator cid;  // Course id.

    // Iterate each student.
    for (student = (*this).unassignedStudents.begin();
            student != (*this).unassignedStudents.end(); ++student) {

        // Iterate each applied course for current student.
        for (cid = (*student).courses.begin();
                cid != (*student).courses.end(); ++student) {

            // Search for applied course in course list.
            for (course = (*this).courses.begin();
                    course != (*this).courses.end(); ++course) { 

                // Find course assign if vacant.
                if ((*course).id.compare(*cid) && (*course).space > 0) {
                    (*course).assignedStudents.push_back(*student);
                }
            }
        }
    }
}
