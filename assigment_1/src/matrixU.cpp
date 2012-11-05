#include "uni.h"


int main(int argc, char *argv[]) {
    bgu = new Uni("./courses.conf", "./students.conf");

    bgu.readCourses();
    bgu.readStudents();

    bgu.assignStudents();
    bgu.printAssignment("./courses.out", "./students.out");
}
