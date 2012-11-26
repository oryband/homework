SHELL = /bin/sh


.SUFFIXES:
.SUFFIXES: .cpp .o


INCLUDES = -I./include
OPENCV_LIB =  -L/usr/include/opencv -L/usr/local/include/opencv -L/usr/lib -L/usr/local/lib -lopencv_core -lopencv_highgui -lopencv_imgproc 

SRCDIR = ./src/
BINDIR = ./bin/

CC = g++
CPPFLAGS = -Wall -Weffc++ -g $(INCLUDES)


all: $(BINDIR)util.o  $(BINDIR)Student.o $(BINDIR)CsStudent.o $(BINDIR)PgStudent.o $(BINDIR)Course.o $(BINDIR)CsCourse.o $(BINDIR)PgCourse.o $(BINDIR)ElCourse.o  $(BINDIR)ImageLoader.o $(BINDIR)ImageOperations.o $(BINDIR)Uni.o $(BINDIR)RandomU.o
	@echo -n 'Building target all: '
	$(CC) $(CPPFLAGS) $(OPENCV_LIB) -o $(BINDIR)RandomU $(BINDIR)util.o $(BINDIR)ImageLoader.o $(BINDIR)ImageOperations.o $(BINDIR)Student.o $(BINDIR)CsStudent.o $(BINDIR)PgStudent.o $(BINDIR)Course.o $(BINDIR)CsCourse.o $(BINDIR)PgCourse.o $(BINDIR)ElCourse.o $(BINDIR)ImageLoader.o $(BINDIR)ImageOperations.o $(BINDIR)Uni.o $(BINDIR)RandomU.o



$(BINDIR)RandomU.o: $(SRCDIR)RandomU.cpp $(BINDIR)util.o $(BINDIR)Student.o $(BINDIR)CsStudent.o $(BINDIR)PgStudent.o $(BINDIR)Course.o $(BINDIR)CsCourse.o $(BINDIR)PgCourse.o  $(BINDIR)ElCourse.o
	@echo -n 'Building target RandomU.o: '
	$(CC) -c $(CPPFLAGS) -o $(BINDIR)RandomU.o $(SRCDIR)RandomU.cpp


$(BINDIR)Uni.o: $(SRCDIR)Uni.cpp $(BINDIR)util.o $(BINDIR)ImageLoader.o $(BINDIR)ImageOperations.o $(BINDIR)Student.o $(BINDIR)CsStudent.o $(BINDIR)PgStudent.o $(BINDIR)Course.o $(BINDIR)CsCourse.o $(BINDIR)PgCourse.o $(BINDIR)ElCourse.o
	@echo -n 'Building target Uni.o: '
	$(CC) -c $(CPPFLAGS) -o $(BINDIR)Uni.o $(SRCDIR)Uni.cpp

$(BINDIR)ImageLoader.o: $(SRCDIR)ImageLoader.cpp
	@echo -n 'Building ImageLoader.o: '
	$(CC) -c $(CPPFLAGS) -o $(BINDIR)ImageLoader.o $(SRCDIR)ImageLoader.cpp

$(BINDIR)ImageOperations.o: $(SRCDIR)ImageOperations.cpp
	@echo -n 'Building target ImageOperations.o: '
	$(CC) -c $(CPPFLAGS) -o $(BINDIR)ImageOperations.o $(SRCDIR)ImageOperations.cpp


$(BINDIR)util.o: $(SRCDIR)util.cpp
	@echo -n 'Building target util.o: '
	$(CC) -c $(CPPFLAGS) -o $(BINDIR)util.o $(SRCDIR)util.cpp


$(BINDIR)CsStudent.o: $(SRCDIR)CsStudent.cpp $(BINDIR)util.o $(BINDIR)Student.o
	@echo -n 'Building target CsStudent.o: '
	$(CC) -c $(CPPFLAGS) -o $(BINDIR)CsStudent.o $(SRCDIR)CsStudent.cpp

$(BINDIR)PgStudent.o: $(SRCDIR)PgStudent.cpp $(BINDIR)util.o $(BINDIR)Student.o
	@echo -n 'Building target PgStudent.o: '
	$(CC) -c $(CPPFLAGS) -o $(BINDIR)PgStudent.o $(SRCDIR)PgStudent.cpp

$(BINDIR)Student.o: $(SRCDIR)Student.cpp
	@echo -n 'Building target Student.o: '
	$(CC) -c $(CPPFLAGS) -o $(BINDIR)Student.o $(SRCDIR)Student.cpp


$(BINDIR)CsCourse.o: $(SRCDIR)CsCourse.cpp $(BINDIR)util.o $(BINDIR)Course.o 
	@echo -n 'Building target CsCourse.o: '
	$(CC) -c $(CPPFLAGS) -o $(BINDIR)CsCourse.o $(SRCDIR)CsCourse.cpp

$(BINDIR)PgCourse.o: $(SRCDIR)PgCourse.cpp $(BINDIR)util.o $(BINDIR)Course.o 
	@echo -n 'Building target PgCourse.o: '
	$(CC) -c $(CPPFLAGS) -o $(BINDIR)PgCourse.o $(SRCDIR)PgCourse.cpp

$(BINDIR)ElCourse.o: $(SRCDIR)ElCourse.cpp $(BINDIR)util.o $(BINDIR)Course.o 
	@echo -n 'Building target ElCourse.o: '
	$(CC) -c $(CPPFLAGS) -o $(BINDIR)ElCourse.o $(SRCDIR)ElCourse.cpp

$(BINDIR)Course.o: $(SRCDIR)Course.cpp
	@echo -n 'Building target Course.o: '
	$(CC) -c $(CPPFLAGS) -o $(BINDIR)Course.o $(SRCDIR)Course.cpp


.PHONY: clean
clean:
	@echo -n 'Cleaning $(BINDIR)'
	rm -fR $(BINDIR)*
