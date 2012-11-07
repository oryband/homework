#include "Uni.h"


using namespace std;


int main(int argc, char *argv[]) {
    Uni* bgu = new Uni("../bin/courses.conf", "../bin/students.conf");

    bgu->assignStudents();
    //bgu->printAssignment("./courses.out", "./students.out");
}
