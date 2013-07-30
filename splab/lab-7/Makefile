SHELL = /bin/sh

.SUFFIXES:
.SUFFIXES: .c .o

SRCDIR = ./src/
INCLUDES = -I$(SRCDIR)
BINDIR = ./bin/

CC = gcc
CFLAGS = -m32 -ansi -Wall -g $(INCLUDES)


all: bin $(BINDIR)hexeditplus.o $(BINDIR)is_palindrome.o
	@echo -n 'Building target all: '
	$(CC) $(CFLAGS) -o $(BINDIR)hexeditplus $(BINDIR)hexeditplus.o
	$(CC) $(CFLAGS) -o $(BINDIR)is_palindrome $(BINDIR)is_palindrome.o

bin:
	@echo -n 'Creating bin/: '
	mkdir -p $(BINDIR)

$(BINDIR)hexeditplus.o: bin $(SRCDIR)hexeditplus.c
	@echo -n 'Building target hexeditplus.o: '
	$(CC) $(CFLAGS) -c -o $(BINDIR)hexeditplus.o $(SRCDIR)hexeditplus.c

$(BINDIR)is_palindrome.o: bin $(SRCDIR)is_palindrome.c
	@echo -n 'Building target is_palindrome.o: '
	$(CC) $(CFLAGS) -c -o $(BINDIR)is_palindrome.o $(SRCDIR)is_palindrome.c


.PHONY: clean
clean:
	@echo -n 'Cleaning $(BINDIR): '
	rm -f $(BINDIR)*

