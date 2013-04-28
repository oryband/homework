SHELL = /bin/sh

.SUFFIXES:
.SUFFIXES: .c .o

SRCDIR = ./src/
INCLUDES = -I$(SRCDIR)
BINDIR = ./bin/

CC = gcc
CFLAGS = -m32 -Wall -ansi -nostdlib -fno-stack-protector $(INCLUDES)

AS = nasm
ASFLAGS = -f elf -w+all -g

LD = ld
LDFLAGS = -melf_i386


all: bin $(BINDIR)main.o $(BINDIR)util.o $(BINDIR)start.o
	@echo -n 'Building target all: '
	$(LD) $(LDFLAGS) $(BINDIR)main.o $(BINDIR)util.o $(BINDIR)start.o -o $(BINDIR)task0


bin:
	@echo -n 'Creating bin/: '
	mkdir -p $(BINDIR)


$(BINDIR)main.o: bin $(SRCDIR)main.c
	@echo -n 'Building target main.o: '
	$(CC) $(CFLAGS) -c -o $(BINDIR)main.o $(SRCDIR)main.c

$(BINDIR)util.o: bin $(SRCDIR)util.c
	@echo -n 'Building target util.o: '
	$(CC) $(CFLAGS) -c -o $(BINDIR)util.o $(SRCDIR)util.c

$(BINDIR)start.o: bin $(SRCDIR)start.s
	@echo -n 'Building target start.o: '
	$(AS) $(ASFLAGS) -o $(BINDIR)start.o $(SRCDIR)start.s


.PHONY: clean
clean:
	@echo -n 'Cleaning $(BINDIR): '
	rm -f $(BINDIR)*.o $(BINDIR)task0
