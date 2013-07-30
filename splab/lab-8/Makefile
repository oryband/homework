SHELL = /bin/sh

.SUFFIXES:
.SUFFIXES: .c .o

SRCDIR = ./src/
INCLUDES = -I$(SRCDIR)
BINDIR = ./bin/

CC = gcc
CFLAGS = -m32 -ansi -Wall -g $(INCLUDES)


all: bin $(BINDIR)examine.o $(BINDIR)can_link.o
	@echo -n 'Building target all: '
	$(CC) $(CFLAGS) -o $(BINDIR)examine $(BINDIR)examine.o
	$(CC) $(CFLAGS) -o $(BINDIR)can_link $(BINDIR)can_link.o

bin:
	@echo -n 'Creating bin/: '
	mkdir -p $(BINDIR)

$(BINDIR)examine.o: bin $(SRCDIR)examine.c
	@echo -n 'Building target examine.o: '
	$(CC) $(CFLAGS) -c -o $(BINDIR)examine.o $(SRCDIR)examine.c

$(BINDIR)can_link.o: bin $(SRCDIR)can_link.c
	@echo -n 'Building target can_link.o: '
	$(CC) $(CFLAGS) -c -o $(BINDIR)can_link.o $(SRCDIR)can_link.c


.PHONY: clean
clean:
	@echo -n 'Cleaning $(BINDIR): '
	rm -f $(BINDIR)*

