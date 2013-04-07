SHELL = /bin/sh

.SUFFIXES:
.SUFFIXES: .c .o

SRCDIR = ./src/
BINDIR = ./bin/

CC = gcc
CFLAGS = -m32 -ansi -Wall -g


all: bin $(BINDIR)count-words.o $(BINDIR)addresses.o $(BINDIR)oop.o
	@echo -n 'Building target all: '
	$(CC) $(CFLAGS) -o $(BINDIR)count-words $(BINDIR)count-words.o
	$(CC) $(CFLAGS) -o $(BINDIR)addresses $(BINDIR)addresses.o
	$(CC) $(CFLAGS) -o $(BINDIR)oop $(BINDIR)oop.o

bin:
	@echo -n 'Creating bin/: '
	mkdir -p $(BINDIR)

$(BINDIR)count-words.o: bin $(SRCDIR)count-words.c
	@echo -n 'Building target count-words.o: '
	$(CC) $(CFLAGS) -c -o $(BINDIR)count-words.o $(SRCDIR)count-words.c

$(BINDIR)addresses.o: bin $(SRCDIR)addresses.c
	@echo -n 'Building target addresses.o: '
	$(CC) $(CFLAGS) -c -o $(BINDIR)addresses.o $(SRCDIR)addresses.c

$(BINDIR)oop.o: bin $(SRCDIR)oop.c
	@echo -n 'Building target oop.o: '
	$(CC) $(CFLAGS) -c -o $(BINDIR)oop.o $(SRCDIR)oop.c

.PHONY: clean
clean:
	@echo -n 'Cleaning $(BINDIR): '
	rm -f $(BINDIR)*.o $(BINDIR)count-words $(BINDIR)addresses $(BINDIR)oop $(BINDIR)run
