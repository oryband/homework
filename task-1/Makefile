SHELL = /bin/sh

.SUFFIXES:
.SUFFIXES: .c .o

SRCDIR = ./src/
BINDIR = ./bin/

CC = gcc
AS = nasm
CFLAGS = -Wall -m32 -ansi -g
ASFLAGS = -f elf -w+all -g


all: bin $(BINDIR)main.o $(BINDIR)numbers.o $(BINDIR)add.o
	@echo -n 'Building target all: '
	$(CC) $(CFLAGS) -o $(BINDIR)run $(BINDIR)main.o $(BINDIR)numbers.o $(BINDIR)add.o


bin:
	@echo -n 'Creating bin/: '
	mkdir -p $(BINDIR)


$(BINDIR)main.o: bin $(SRCDIR)main.c
	@echo -n 'Building target main.o: '
	$(CC) $(CFLAGS) -c -o $(BINDIR)main.o $(SRCDIR)main.c

$(BINDIR)numbers.o: bin $(SRCDIR)numbers.c
	@echo -n 'Building target numbers.o: '
	$(CC) $(CFLAGS) -c -o $(BINDIR)numbers.o $(SRCDIR)numbers.c

$(BINDIR)add.o: bin $(SRCDIR)add.s
	@echo -n 'Building target add.o: '
	$(AS) $(ASFLAGS) -o $(BINDIR)add.o $(SRCDIR)add.s


.PHONY: clean
clean:
	@echo -n 'Cleaning $(BINDIR): '
	rm -f $(BINDIR)*.o $(BINDIR)run
