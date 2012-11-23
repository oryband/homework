#ifndef CONSTS_H_
#define CONSTS_H_


#define COURSES_FILE "../courses.conf"
#define STUDENTS_FILE "students.conf"
#define CURRICULUM_FILE "curriculum.conf"
#define RANDOM_FILE "random.log"

#define CS "CS"
#define PG "PG"
#define ELECTIVE "ELECTIVE"

#define CS_QUIT_CHANCE 25
#define PG_QUIT_CHANCE 20

#define GRADE_RANGE 101

#define PROFILE_IMAGE_SIZE 100

// 1 - Student is taking a course.
// 2 - Student took and finish course successfully.
// 3 - Student took and finish course unsuccessfully.
// 4 - Student has graduated.
// 5 - Student has not graduated.
#define TAKING_COURSE 1
#define FINISHED_COURSE 2
#define FAILED_COURSE 3
#define GRADUATED 4
#define NOT_GRADUATED 5
#define DENIED 6
 

#endif
