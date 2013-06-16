SHELL = /bin/sh

.SUFFIXES:
.SUFFIXES: .c .o

SRCDIR = ./src/
INCLUDES = -I$(SRCDIR)
BINDIR = ./bin/

CC = gcc
CFLAGS = -m32 -ansi -Wall -g $(INCLUDES)


all: bin $(BINDIR)apply_func.o
	@echo -n 'Building target all: '
	$(CC) $(CFLAGS) -o $(BINDIR)apply_func $(BINDIR)apply_func.o

bin:
	@echo -n 'Creating bin/: '
	mkdir -p $(BINDIR)

$(BINDIR)apply_func.o: bin $(SRCDIR)apply_func.c
	@echo -n 'Building target apply_func.o: '
	$(CC) $(CFLAGS) -c -o $(BINDIR)apply_func.o $(SRCDIR)apply_func.c


.PHONY: clean
clean:
	@echo -n 'Cleaning $(BINDIR): '
	rm -f $(BINDIR)*

