#ifndef UTIL_H_
#define UTIL_H_


#include <string>
#include <vector>
#include <iostream>
#include <fstream>
#include <cstdlib>

#include "consts.h"


void getLines(
        std::string filePath,
        std::vector< std::vector<std::string> > &lines);

void writeNumOfSemesterToFile(int semester);

void writeToStudentsLogFile(
        std::string studentId,
        std::string courseName,
        std::string department,
        int status);

#endif
