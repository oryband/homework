SHELL = /bin/sh

.SUFFIXES:
.SUFFIXES: .cpp .o

CC = g++
CPPFLAGS  = -Wall -Weffc++

SRCDIR = ./
BINDIR = $(SRCDIR)bin/


all: base2 print_words

base2:
	@echo 'Building target: base2'
	@echo 'Invoking: C++ Linker'
	mkdir -p $(BINDIR)
	$(CC) $(CPPFLAGS) base2.cpp -o $(BINDIR)base2
	@echo 'Finished building target: base'
	@echo ' '

print_words:
	@echo 'Building target: print_words'
	@echo 'Invoking: C++ Linker'
	mkdir -p $(BINDIR)
	$(CC) $(CPPFLAGS) print_words.cpp -o $(BINDIR)print_words
	@echo 'Finished building target: print_words'
	@echo ' '

clean: 
	rm -f $(BINDIR)base2 $(BINDIR)print_words
	rm -d $(BINDIR)
