SHELL = /bin/sh

.SUFFIXES:
.SUFFIXES: .c .o

SRCDIR = ./src/
INCLUDES = -I$(SRCDIR)
BINDIR = ./bin/

CC = gcc
CFLAGS = -m32 -ansi -Wall -g $(INCLUDES)


all: bin $(BINDIR)mypipe.o
	@echo -n 'Building target all: '
	$(CC) $(CFLAGS) -o $(BINDIR)mypipe $(BINDIR)mypipe.o

bin:
	@echo -n 'Creating bin/: '
	mkdir -p $(BINDIR)

$(BINDIR)mypipe.o: bin $(SRCDIR)mypipe.c
	@echo -n 'Building target mypipe.o: '
	$(CC) $(CFLAGS) -c -o $(BINDIR)mypipe.o $(SRCDIR)mypipe.c


.PHONY: clean
clean:
	@echo -n 'Cleaning $(BINDIR): '
	rm -f $(BINDIR)*
