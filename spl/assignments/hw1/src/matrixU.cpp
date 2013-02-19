/** SPL Assignment #1
 * http://www.cs.bgu.ac.il/~spl131/Assignments/Assignment_1
 *
 * Eldar Damari damariel@post.bgu.ac.il
 * Ory Band oryb@post.bgu.acil
 */

#include "Uni.h"


using namespace std;


int main(int argc, char *argv[]) {
    Uni bgu = Uni("courses.conf", "students.conf");

    bgu.assignStudents();
    bgu.printAssignment("courses.out", "students.out");
}
