SHELL = /bin/sh

.SUFFIXES:
.SUFFIXES: .c .o

SRCDIR = ./src/
BINDIR = ./bin/

CC = gcc
CFLAGS = -m32 -ansi -Wall -g


all: bin $(BINDIR)bubblesort.o
	@echo -n 'Building target all: '
	$(CC) $(CFLAGS) -o $(BINDIR)bubblesort $(BINDIR)bubblesort.o

bin:
	@echo -n 'Creating bin/: '
	mkdir -p $(BINDIR)

$(BINDIR)bubblesort.o: bin $(SRCDIR)bubblesort.c
	@echo -n 'Building target bubblesort.o: '
	$(CC) $(CFLAGS) -c -o $(BINDIR)bubblesort.o $(SRCDIR)bubblesort.c

.PHONY: clean
clean:
	@echo -n 'Cleaning $(BINDIR): '
	rm -f $(BINDIR)*.o $(BINDIR)count-words $(BINDIR)addresses $(BINDIR)oop $(BINDIR)run
