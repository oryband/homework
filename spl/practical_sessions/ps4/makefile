SHELL = /bin/sh


.SUFFIXES:
.SUFFIXES: .cpp .o


CC = g++
CPPFLAGS = -Wall -Weffc++ -c -g

INCLUDES = -I/usr/include/opencv -I/usr/local/include/opencv -Iinclude/

SRCDIR = ./src/
BINDIR = ./bin/


all: $(BINDIR)ImageLoader.o $(BINDIR)ImageOperations.o $(BINDIR)run.o
	$(CC) -o $(BINDIR)example $(BINDIR)ImageLoader.o $(BINDIR)ImageOperations.o $(BINDIR)run.o -L/usr/lib -L/usr/local/lib -lopencv_core -lopencv_highgui -lopencv_imgproc

$(BINDIR)ImageLoader.o: $(SRCDIR)ImageLoader.cpp
	$(CC) $(CPPFLAGS) $(INCLUDES) -o $(BINDIR)ImageLoader.o $(SRCDIR)ImageLoader.cpp

$(BINDIR)ImageOperations.o: $(SRCDIR)ImageOperations.cpp
	$(CC) $(CPPFLAGS) $(INCLUDES) -o $(BINDIR)ImageOperations.o $(SRCDIR)ImageOperations.cpp

$(BINDIR)run.o: $(SRCDIR)run.cpp
	$(CC) $(CPPFLAGS) $(INCLUDES) -o $(BINDIR)run.o $(SRCDIR)run.cpp


.PHONY: clean
clean:
	rm -fR $(BINDIR)*
