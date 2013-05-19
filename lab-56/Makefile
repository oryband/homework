SHELL = /bin/sh

.SUFFIXES:
.SUFFIXES: .c .o

SRCDIR = ./src/
INCLUDES = -I$(SRCDIR)
BINDIR = ./bin/

CC = gcc
CFLAGS = -m32 -ansi -Wall -g $(INCLUDES)


all: bin $(BINDIR)LineParser.o $(BINDIR)myshell.o
	@echo -n 'Building target all: '
	$(CC) $(CFLAGS) -o $(BINDIR)myshell $(BINDIR)myshell.o $(BINDIR)LineParser.o

bin:
	@echo -n 'Creating bin/: '
	mkdir -p $(BINDIR)

$(BINDIR)LineParser.o: bin $(SRCDIR)LineParser.c
	@echo -n 'Building target LineParser.o: '
	$(CC) $(CFLAGS) -c -o $(BINDIR)LineParser.o $(SRCDIR)LineParser.c

$(BINDIR)myshell.o: bin $(SRCDIR)myshell.c
	@echo -n 'Building target myshell.o: '
	$(CC) $(CFLAGS) -c -o $(BINDIR)myshell.o $(SRCDIR)myshell.c


.PHONY: clean
clean:
	@echo -n 'Cleaning $(BINDIR): '
	rm -f $(BINDIR)*
