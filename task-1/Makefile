SHELL = /bin/sh


.SUFFIXES:
.SUFFIXES: .c .o


SRCDIR = ./src/
#INCLUDEDIR = ./include/
BINDIR = ./bin/

#INCLUDES = -I$(INCLUDEDIR)
#BOOST = -lboost_system -lboost_thread
#LIBS = -L/usr/local/lib -L/usr/local/include

#CPPFLAGS = -Wall -Weffc++ -g $(INCLUDES)
CFLAGS = -Wall -Weffc++ -g

CC = g++


all: bin main.o numbers.o
	@echo -n 'Building target all: '
	$(CC) $(CFLAGS) -o $(BINDIR)main $(BINDIR)main.o $(BINDIR)numbers.o


bin:
	@echo -n 'Creating bin/: '
	mkdir -p $(BINDIR)


$(BINDIR)main.o: bin $(SRCDIR)main.c
	@echo -n 'Building target main.o: '
	$(CC) $(CFLAGS) -c -o $(BINDIR)main.o $(SRCDIR)main.c


$(BINDIR)numbers.o: bin $(SRCDIR)numbers.c
	@echo -n 'Building target numbers.o: '
	$(CC) $(CFLAGS) -c -o $(BINDIR)numbers.o $(SRCDIR)numbers.c


.PHONY: clean
clean:
	@echo -n 'Cleaning $(BINDIR): '
	rm -f $(BINDIR)*.o $(BINDIR)client
