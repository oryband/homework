SHELL = /bin/sh

.SUFFIXES:
.SUFFIXES: .c .o

SRCDIR = ./src/
BINDIR = ./bin/

CC = gcc
CFLAGS = -m32 -ansi -Wall -g


all: bin $(BINDIR)cycle_split.o
	@echo -n 'Building target all: '
	$(CC) $(CFLAGS) -o $(BINDIR)cycle_split $(BINDIR)cycle_split.o

bin:
	@echo -n 'Creating bin/: '
	mkdir -p $(BINDIR)

$(BINDIR)cycle_split.o: bin $(SRCDIR)cycle_split.c
	@echo -n 'Building target cycle_split.o: '
	$(CC) $(CFLAGS) -c -o $(BINDIR)cycle_split.o $(SRCDIR)cycle_split.c

.PHONY: clean
clean:
	@echo -n 'Cleaning $(BINDIR): '
	rm -f $(BINDIR)*.o $(BINDIR)run
