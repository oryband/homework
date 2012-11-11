#include "Uni.h"


using namespace std;


int main(int argc, char *argv[]) {
    Uni bgu = Uni("courses.conf", "students.conf");

    bgu.assignStudents();
    bgu.printAssignment("courses.out", "students.out");
}
