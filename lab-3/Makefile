SHELL = /../Makefile

.SUFFIXES:
.SUFFIXES: .c .o

SRCDIR = ./src/
BINDIR = ./bin/

CC = gcc
CFLAGS = -m32 -ansi -Wall -g


all: bin $(BINDIR)bubblesort.o list.o
	@echo -n 'Building target all: '
	$(CC) $(CFLAGS) -o $(BINDIR)bubblesort $(BINDIR)bubblesort.o
	$(CC) $(CFLAGS) -o $(BINDIR)list $(BINDIR)list.o

bin:
	@echo -n 'Creating bin/: '
	mkdir -p $(BINDIR)

$(BINDIR)bubblesort.o: bin $(SRCDIR)bubblesort.c
	@echo -n 'Building target bubblesort.o: '
	$(CC) $(CFLAGS) -c -o $(BINDIR)bubblesort.o $(SRCDIR)bubblesort.c

$(BINDIR)list.o: bin $(SRCDIR)list.c
	@echo -n 'Building target list.o: '
	$(CC) $(CFLAGS) -c -o $(BINDIR)list.o $(SRCDIR)list.c

.PHONY: clean
clean:
	@echo -n 'Cleaning $(BINDIR): '
	rm -f $(BINDIR)*.o $(BINDIR)count-words $(BINDIR)addresses $(BINDIR)oop $(BINDIR)run
