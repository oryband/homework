SHELL = /bin/sh


.SUFFIXES:
.SUFFIXES: .cpp .o


CC = g++
CPPFLAGS = -Wall -Weffc++ -g

INCLUDES = -I/usr/include/opencv -I/usr/local/include/opencv -Iinclude/

SRCDIR = ./src/
BINDIR = ./bin/


all: $(BINDIR)util.o $(BINDIR)Student.o $(BINDIR)CsStudent.o $(BINDIR)PgStudent.o $(BINDIR)Course.o $(BINDIR)CsCourse.o $(BINDIR)PgCourse.o $(BINDIR)ElCourse.o $(BINDIR)Uni.o $(BINDIR)RandomU.o
	@echo -n 'Buildting target all: '
	$(CC) -o $(BINDIR)RandomU $(BINDIR)util.o $(BINDIR)Student.o $(BINDIR)CsStudent.o $(BINDIR)PgStudent.o $(BINDIR)Course.o $(BINDIR)CsCourse.o $(BINDIR)PgCourse.o $(BINDIR)ElCourse.o $(BINDIR)Uni.o $(BINDIR)RandomU.o -L/usr/lib -L/usr/local/lib -lopencv_core -lopencv_highgui -lopencv_imgproc


$(BINDIR)RandomU.o: $(SRCDIR)RandomU.cpp $(BINDIR)util.o $(BINDIR)Student.o $(BINDIR)CsStudent.o $(BINDIR)PgStudent.o $(BINDIR)Course.o $(BINDIR)CsCourse.o $(BINDIR)PgCourse.o  $(BINDIR)ElCourse.o
	@echo -n 'Buildting target RandomU.o: '
	$(CC) -c $(CPPFLAGS) $(INCLUDES) -o $(BINDIR)RandomU.o $(SRCDIR)RandomU.cpp


$(BINDIR)Uni.o: $(SRCDIR)Uni.cpp $(BINDIR)util.o $(BINDIR)Student.o $(BINDIR)CsStudent.o $(BINDIR)PgStudent.o $(BINDIR)Course.o $(BINDIR)CsCourse.o $(BINDIR)PgCourse.o $(BINDIR)ElCourse.o
	@echo -n 'Buildting target Uni.o: '
	$(CC) -c $(CPPFLAGS) $(INCLUDES) -o $(BINDIR)Uni.o $(SRCDIR)Uni.cpp


$(BINDIR)util.o: $(SRCDIR)util.cpp
	@echo -n 'Buildting target util.o: '
	$(CC) -c $(CPPFLAGS) $(INCLUDES) -o $(BINDIR)util.o $(SRCDIR)util.cpp


$(BINDIR)CsStudent.o: $(SRCDIR)CsStudent.cpp $(BINDIR)util.o $(BINDIR)Student.o
	@echo -n 'Buildting target CsStudent.o: '
	$(CC) -c $(CPPFLAGS) $(INCLUDES) -o $(BINDIR)CsStudent.o $(SRCDIR)CsStudent.cpp

$(BINDIR)PgStudent.o: $(SRCDIR)PgStudent.cpp $(BINDIR)util.o $(BINDIR)Student.o
	@echo -n 'Buildting target PgStudent.o: '
	$(CC) -c $(CPPFLAGS) $(INCLUDES) -o $(BINDIR)PgStudent.o $(SRCDIR)PgStudent.cpp

$(BINDIR)Student.o: $(SRCDIR)Student.cpp
	@echo -n 'Buildting target Student.o: '
	$(CC) -c $(CPPFLAGS) $(INCLUDES) -o $(BINDIR)Student.o $(SRCDIR)Student.cpp


$(BINDIR)CsCourse.o: $(SRCDIR)CsCourse.cpp $(BINDIR)util.o $(BINDIR)Course.o 
	@echo -n 'Buildting target CsCourse.o: '
	$(CC) -c $(CPPFLAGS) $(INCLUDES) -o $(BINDIR)CsCourse.o $(SRCDIR)CsCourse.cpp

$(BINDIR)PgCourse.o: $(SRCDIR)PgCourse.cpp $(BINDIR)util.o $(BINDIR)Course.o 
	@echo -n 'Buildting target PgCourse.o: '
	$(CC) -c $(CPPFLAGS) $(INCLUDES) -o $(BINDIR)PgCourse.o $(SRCDIR)PgCourse.cpp

$(BINDIR)ElCourse.o: $(SRCDIR)ElCourse.cpp $(BINDIR)util.o $(BINDIR)Course.o 
	@echo -n 'Buildting target ElCourse.o: '
	$(CC) -c $(CPPFLAGS) $(INCLUDES) -o $(BINDIR)ElCourse.o $(SRCDIR)ElCourse.cpp

$(BINDIR)Course.o: $(SRCDIR)Course.cpp
	@echo -n 'Buildting target Course.o: '
	$(CC) -c $(CPPFLAGS) $(INCLUDES) -o $(BINDIR)Course.o $(SRCDIR)Course.cpp


.PHONY: clean
clean:
	@echo -n 'Cleaning $(BINDIR)'
	rm -fR $(BINDIR)*
