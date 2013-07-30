SHELL = /bin/sh

.SUFFIXES:
.SUFFIXES: .c .o

SRCDIR = ./src/
BINDIR = ./bin/

CC = gcc
CFLAGS = -m32 -ansi -Wall -g


all: bin $(BINDIR)bubblesort.o $(BINDIR)num_info.o
	@echo -n 'Building target all: '
	$(CC) $(CFLAGS) -o $(BINDIR)bubblesort $(BINDIR)bubblesort.o
	$(CC) $(CFLAGS) -o $(BINDIR)num_info $(BINDIR)num_info.o

bin:
	@echo -n 'Creating bin/: '
	mkdir -p $(BINDIR)

$(BINDIR)bubblesort.o: bin $(SRCDIR)bubblesort.c
	@echo -n 'Building target bubblesort.o: '
	$(CC) $(CFLAGS) -c -o $(BINDIR)bubblesort.o $(SRCDIR)bubblesort.c

$(BINDIR)num_info.o: bin $(SRCDIR)num_info.c
	@echo -n 'Building target num_info.o: '
	$(CC) $(CFLAGS) -c -o $(BINDIR)num_info.o $(SRCDIR)num_info.c

.PHONY: clean
clean:
	@echo -n 'Cleaning $(BINDIR): '
	rm -f $(BINDIR)*.o $(BINDIR)bubblesort $(BINDIR)num_info
