SHELL = /bin/sh

.SUFFIXES:
.SUFFIXES: .c .o

SRCDIR = ./src/
BINDIR = ./bin/

CC = gcc
CFLAGS = -m32 -ansi -Wall -g


all: bin $(BINDIR)count_words.o
	@echo -n 'Building target all: '
	$(CC) $(CFLAGS) -o $(BINDIR)count_words $(BINDIR)count_words.o

bin:
	@echo -n 'Creating bin/: '
	mkdir -p $(BINDIR)

$(BINDIR)count_words.o: bin $(SRCDIR)count_words.c
	@echo -n 'Building target count_words.o: '
	$(CC) $(CFLAGS) -c -o $(BINDIR)count_words.o $(SRCDIR)count_words.c

.PHONY: clean
clean:
	@echo -n 'Cleaning $(BINDIR): '
	rm -f $(BINDIR)*.o $(BINDIR)run
