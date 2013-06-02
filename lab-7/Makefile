SHELL = /bin/sh

.SUFFIXES:
.SUFFIXES: .c .o

SRCDIR = ./src/
INCLUDES = -I$(SRCDIR)
BINDIR = ./bin/

CC = gcc
CFLAGS = -m32 -ansi -Wall -g $(INCLUDES)


all: bin $(BINDIR)hexeditplus.o
	@echo -n 'Building target all: '
	$(CC) $(CFLAGS) -o $(BINDIR)hexeditplus $(BINDIR)hexeditplus.o

bin:
	@echo -n 'Creating bin/: '
	mkdir -p $(BINDIR)

$(BINDIR)hexeditplus.o: bin $(SRCDIR)hexeditplus.c
	@echo -n 'Building target hexeditplus.o: '
	$(CC) $(CFLAGS) -c -o $(BINDIR)hexeditplus.o $(SRCDIR)hexeditplus.c


.PHONY: clean
clean:
	@echo -n 'Cleaning $(BINDIR): '
	rm -f $(BINDIR)*

