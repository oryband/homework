SHELL = /bin/sh


.SUFFIXES:
.SUFFIXES: .cpp .o


CC = g++
CPPFLAGS = -Wall -Weffc++ -g

INCLUDES = -I./include
#OPENCV_LIB = -L/usr/include/opencv -L/usr/local/include/opencv -L/usr/local/lib -lopencv_core -lopencv_highgui -lopencv_imgproc
OPENCV_LIB = -L/usr/lib -lopencv_core -lopencv_highgui -lopencv_imgproc -I/usr/include/opencv -L/usr/local/include/opencv -L/usr/local/lib 

SRCDIR = ./src/
BINDIR = ./bin/


all: $(BINDIR)util.o $(BINDIR)Student.o $(BINDIR)CsStudent.o $(BINDIR)PgStudent.o $(BINDIR)Course.o $(BINDIR)CsCourse.o $(BINDIR)PgCourse.o $(BINDIR)ElCourse.o $(BINDIR)ImageLoader.o $(BINDIR)ImageOperations.o $(BINDIR)Uni.o $(BINDIR)RandomU.o
	@echo -n 'Buildting target all: '
	$(CC) -o $(BINDIR)RandomU $(BINDIR)util.o $(BINDIR)Student.o $(BINDIR)CsStudent.o $(BINDIR)PgStudent.o $(BINDIR)Course.o $(BINDIR)CsCourse.o $(BINDIR)PgCourse.o $(BINDIR)ElCourse.o $(BINDIR)ImageLoader.o $(BINDIR)ImageOperations.o $(BINDIR)Uni.o $(BINDIR)RandomU.o $(OPENCV_LIB)


$(BINDIR)RandomU.o: $(SRCDIR)RandomU.cpp $(BINDIR)util.o $(BINDIR)Student.o $(BINDIR)CsStudent.o $(BINDIR)PgStudent.o $(BINDIR)Course.o $(BINDIR)CsCourse.o $(BINDIR)PgCourse.o  $(BINDIR)ElCourse.o
	@echo -n 'Buildting target RandomU.o: '
	$(CC) -c $(CPPFLAGS) $(INCLUDES) -o $(BINDIR)RandomU.o $(SRCDIR)RandomU.cpp


$(BINDIR)Uni.o: $(SRCDIR)Uni.cpp $(BINDIR)util.o $(BINDIR)Student.o $(BINDIR)CsStudent.o $(BINDIR)PgStudent.o $(BINDIR)Course.o $(BINDIR)CsCourse.o $(BINDIR)PgCourse.o $(BINDIR)ElCourse.o $(BINDIR)ImageLoader.o $(BINDIR)ImageOperations.o  
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

$(BINDIR)ImageLoader.o: $(SRCDIR)ImageLoader.cpp
	@echo -n 'Buildting  ImageLoader.o: '
	$(CC) -c $(CPPFLAGS) $(INCLUDES) -c $(OPENCV_LIB) -o $(BINDIR)ImageLoader.o $(SRCDIR)ImageLoader.cpp

$(BINDIR)ImageOperations.o: $(SRCDIR)ImageOperations.cpp
	@echo -n 'Buildting target ImageOperations.o: '
	$(CC) -c $(CPPFLAGS) $(INCLUDES) -c $(OPENCV_LIB) -o $(BINDIR)ImageOperations.o $(SRCDIR)ImageOperations.cpp

.PHONY: clean
clean:
	@echo -n 'Cleaning $(BINDIR)'
	rm -fR $(BINDIR)*
