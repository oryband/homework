SHELL = /bin/sh


.SUFFIXES:
.SUFFIXES: .cpp .o


CC = g++
CPPFLAGS = -Wall -Weffc++ -c -g

INCLUDES = -I/usr/include/opencv -I/usr/local/include/opencv -Iinclude/

SRCDIR = ./src/
BINDIR = ./bin/


all: $(BINDIR)Student.o $(BINDIR)CsStudent.o $(BINDIR)PgStudent.o $(BINDIR)Course.o $(BINDIR)CsCourse.o $(BINDIR)PgCourse.o $(BINDIR)Uni.o $(BINDIR)RandomU.o
	$(CC) -o $(BINDIR)RandomU $(BINDIR)Student.o $(BINDIR)CsStudent.o $(BINDIR)PgStudent.o $(BINDIR)Course.o $(BINDIR)CsCourse.o $(BINDIR)PgCourse.o $(BINDIR)Uni.o $(BINDIR)RandomU.o -L/usr/lib -L/usr/local/lib -lopencv_core -lopencv_highgui -lopencv_imgproc

$(BINDIR)Student.o: $(SRCDIR)Student.cpp
	$(CC) $(CPPFLAGS) $(INCLUDES) -o $(BINDIR)Student.o $(SRCDIR)Student.cpp

$(BINDIR)CsStudent.o: $(SRCDIR)CsStudent.cpp $(BINDIR)Student.o
	$(CC) $(CPPFLAGS) $(INCLUDES) -o $(BINDIR)CsStudent.o $(SRCDIR)CsStudent.cpp $(SRCDIR)Student.o

$(BINDIR)PgStudent.o: $(SRCDIR)PgStudent.cpp $(BINDIR)Student.o
	$(CC) $(CPPFLAGS) $(INCLUDES) -o $(BINDIR)PgStudent.o $(SRCDIR)PgStudent.cpp $(SRCDIR)Student.o







$(BINDIR)RandomU.o: $(SRCDIR)Random.cpp $(BINDIR)Student.o $(BINDIR)CsStudent.o $(BINDIR)PgStudent.o $(BINDIR)Course.o $(BINDIR)CsCourse.o $(BINDIR)PgCourse.o
	$(CC) $(CPPFLAGS) $(INCLUDES) -o $(BINDIR)RandomU.o $(SRCDIR)RandomU.cpp Student.o CsStudent.o PgStudent.o Course.o CsCourse.o PgCourse.o
 


.PHONY: clean
clean:
	rm -fR $(BINDIR)*
