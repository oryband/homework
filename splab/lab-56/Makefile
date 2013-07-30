SHELL = /bin/sh

.SUFFIXES:
.SUFFIXES: .c .o

SRCDIR = ./src/
INCLUDES = -I$(SRCDIR)
BINDIR = ./bin/

CC = gcc
CFLAGS = -m32 -ansi -Wall -g $(INCLUDES)


all: bin $(BINDIR)LineParser.o $(BINDIR)myshell.o $(BINDIR)mypipe.o $(BINDIR)mypipeline.o
	@echo -n 'Building target all: '
	$(CC) $(CFLAGS) -o $(BINDIR)myshell $(BINDIR)myshell.o $(BINDIR)LineParser.o
	$(CC) $(CFLAGS) -o $(BINDIR)mypipe $(BINDIR)mypipe.o
	$(CC) $(CFLAGS) -o $(BINDIR)mypipeline $(BINDIR)mypipeline.o

bin:
	@echo -n 'Creating bin/: '
	mkdir -p $(BINDIR)

$(BINDIR)LineParser.o: bin $(SRCDIR)LineParser.c
	@echo -n 'Building target LineParser.o: '
	$(CC) $(CFLAGS) -c -o $(BINDIR)LineParser.o $(SRCDIR)LineParser.c

$(BINDIR)myshell.o: bin $(SRCDIR)myshell.c
	@echo -n 'Building target myshell.o: '
	$(CC) $(CFLAGS) -c -o $(BINDIR)myshell.o $(SRCDIR)myshell.c

$(BINDIR)mypipe.o: bin $(SRCDIR)mypipe.c
	@echo -n 'Building target mypipe.o: '
	$(CC) $(CFLAGS) -c -o $(BINDIR)mypipe.o $(SRCDIR)mypipe.c

$(BINDIR)mypipeline.o: bin $(SRCDIR)mypipeline.c
	@echo -n 'Building target mypipeline.o: '
	$(CC) $(CFLAGS) -c -o $(BINDIR)mypipeline.o $(SRCDIR)mypipeline.c


.PHONY: clean
clean:
	@echo -n 'Cleaning $(BINDIR): '
	rm -f $(BINDIR)*
