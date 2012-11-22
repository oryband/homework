SHELL = /bin/sh


.SUFFIXES:
.SUFFIXES: .cpp .o


CC = g++
CPPFLAGS = -Wall -Weffc++ -c -g

INCLUDES = -I/usr/include/opencv -I/usr/local/include/opencv -Iinclude/

SRCDIR = ./src/
BINDIR = ./bin/


all: $(BINDIR)Student.o $(BINDIR)CsStudent.o $(BINDIR)PgStudent.o $(BINDIR)Course.o $(BINDIR)CsCourse.o $(BINDIR)PgCourse.o $(BINDIR)Uni.o $(BINDIR)RandomU.o
	@echo 'Building target: all'
	$(CC) -o $(BINDIR)RandomU $(BINDIR)Student.o $(BINDIR)CsStudent.o $(BINDIR)PgStudent.o $(BINDIR)Course.o $(BINDIR)CsCourse.o $(BINDIR)PgCourse.o $(BINDIR)Uni.o $(BINDIR)RandomU.o -L/usr/lib -L/usr/local/lib -lopencv_core -lopencv_highgui -lopencv_imgproc

$(BINDIR)Student.o: $(SRCDIR)Student.cpp
	@echo 'Building target: Student.o'
	$(CC) $(CPPFLAGS) $(INCLUDES) -o $(BINDIR)Student.o $(SRCDIR)Student.cpp

$(BINDIR)CsStudent.o: $(SRCDIR)CsStudent.cpp $(BINDIR)Student.o
	@echo 'Building target: CsStudent.o'
	$(CC) $(CPPFLAGS) $(INCLUDES) -o $(BINDIR)CsStudent.o $(SRCDIR)CsStudent.cpp $(BINDIR)Student.o

$(BINDIR)PgStudent.o: $(SRCDIR)PgStudent.cpp $(BINDIR)Student.o
	@echo 'Building target: PgStudent.o'
	$(CC) $(CPPFLAGS) $(INCLUDES) -o $(BINDIR)PgStudent.o $(SRCDIR)PgStudent.cpp $(BINDIR)Student.o

$(BINDIR)Course.o: $(SRCDIR)Course.cpp
	@echo 'Building target: Course.o'
	$(CC) $(CPPFLAGS) $(INCLUDES) -o $(BINDIR)Course.o $(SRCDIR)Course.cpp

$(BINDIR)CsCourse.o: $(SRCDIR)CsCourse.cpp $(BINDIR)Course.o 
	@echo 'Building target: CsCourse.o'
	$(CC) $(CPPFLAGS) $(INCLUDES) -o $(BINDIR)CsCourse.o $(SRCDIR)CsCourse.cpp $(BINDIR)Course.o 

$(BINDIR)PgCourse.o: $(SRCDIR)PgCourse.cpp $(BINDIR)Course.o 
	@echo 'Building target: PgCourse.o'
	$(CC) $(CPPFLAGS) $(INCLUDES) -o $(BINDIR)PgCourse.o $(SRCDIR)PgCourse.cpp $(BINDIR)Course.o 

$(BINDIR)Uni.o: $(SRCDIR)Uni.cpp $(BINDIR)Student.o $(BINDIR)CsStudent.o $(BINDIR)PgStudent.o $(BINDIR)Course.o $(BINDIR)CsCourse.o $(BINDIR)PgCourse.o
	@echo 'Building target: Uni.o'
	$(CC) $(CPPFLAGS) $(INCLUDES) -o $(BINDIR)Uni.o $(SRCDIR)Uni.cpp $(BINDIR)Student.o $(BINDIR)CsStudent.o $(BINDIR)PgStudent.o $(BINDIR)Course.o $(BINDIR)CsCourse.o $(BINDIR)PgCourse.o

$(BINDIR)RandomU.o: $(SRCDIR)RandomU.cpp $(BINDIR)Student.o $(BINDIR)CsStudent.o $(BINDIR)PgStudent.o $(BINDIR)Course.o $(BINDIR)CsCourse.o $(BINDIR)PgCourse.o
	@echo 'Building target: RandomU.o'
	$(CC) $(CPPFLAGS) $(INCLUDES) -o $(BINDIR)RandomU.o $(SRCDIR)RandomU.cpp $(BINDIR)Student.o $(BINDIR)CsStudent.o $(BINDIR)PgStudent.o $(BINDIR)Course.o $(BINDIR)CsCourse.o $(BINDIR)PgCourse.o


.PHONY: clean
clean:
	@echo 'Cleaning $(BINDIR)'
	rm -fR $(BINDIR)*
