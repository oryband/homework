SHELL = /bin/sh

.SUFFIXES:
.SUFFIXES: .c .o

SRCDIR = ./src/
INCLUDES = -I$(SRCDIR)
BINDIR = ./bin/

CC = gcc
CFLAGS = -m32 -ansi -Wall -g $(INCLUDES)

all: bin $(BINDIR)loader.o
	@echo -n 'Building target all: '
	# $(CC) $(CFLAGS) -o $(BINDIR)loader $(BINDIR)loader.o
	ld -o $(BINDIR)loader $(BINDIR)loader.o $(BINDIR)startup.o $(BINDIR)start.o -L/usr/lib32 -lc -T $(SRCDIR)linking_script -dynamic-linker /lib32/ld-linux.so.2

bin:
	@echo -n 'Creating bin/: '
	mkdir -p $(BINDIR)

$(BINDIR)loader.o: bin $(SRCDIR)loader.c
	@echo -n 'Building target loader.o: '
	# $(CC) $(CFLAGS) -c -o $(BINDIR)loader.o $(SRCDIR)loader.c
	$(CC) -m32 -c $(SRCDIR)loader.c -o $(BINDIR)loader.o


.PHONY: clean
clean:
	@echo -n 'Cleaning $(BINDIR): '
	rm -f $(BINDIR)*

