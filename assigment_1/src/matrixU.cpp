#include "Uni.h"


using namespace std;


int main(int argc, char *argv[]) {
    // TODO: Change file paths.
    Uni bgu = Uni("../bin/courses.conf", "../bin/students.conf");

    bgu.assignStudents();
    bgu.printAssignment("../bin/courses.out", "../bin/students.out");
}
