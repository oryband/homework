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


all: bin $(BINDIR)main.o $(BINDIR)hello-world.o $(BINDIR)my_cut.o $(BINDIR)flame2.o $(BINDIR)util.o $(BINDIR)start.o
	@echo -n 'Building target all: '
	$(LD) $(LDFLAGS) $(BINDIR)main.o $(BINDIR)util.o $(BINDIR)start.o -o $(BINDIR)task0
	$(LD) $(LDFLAGS) $(BINDIR)hello-world.o $(BINDIR)util.o $(BINDIR)start.o -o $(BINDIR)hello-world
	$(LD) $(LDFLAGS) $(BINDIR)my_cut.o $(BINDIR)util.o $(BINDIR)start.o -o $(BINDIR)my_cut
	$(LD) $(LDFLAGS) $(BINDIR)flame2.o $(BINDIR)util.o $(BINDIR)start.o -o $(BINDIR)flame2


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


$(BINDIR)hello-world.o: bin $(SRCDIR)hello-world.c
	@echo -n 'Building target hello-world.o: '
	$(CC) $(CFLAGS) -c -o $(BINDIR)hello-world.o $(SRCDIR)hello-world.c

$(BINDIR)my_cut.o: bin $(SRCDIR)my_cut.c
	@echo -n 'Building target my_cut.o: '
	$(CC) $(CFLAGS) -c -o $(BINDIR)my_cut.o $(SRCDIR)my_cut.c

$(BINDIR)flame2.o: bin $(SRCDIR)flame2.c
	@echo -n 'Building target flame2.o: '
	$(CC) $(CFLAGS) -c -o $(BINDIR)flame2.o $(SRCDIR)flame2.c


.PHONY: clean
clean:
	@echo -n 'Cleaning $(BINDIR): '
	rm -f $(BINDIR)*
