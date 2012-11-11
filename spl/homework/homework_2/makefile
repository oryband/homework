SHELL = /bin/sh

.SUFFIXES:
.SUFFIXES: .cpp .o

CC = g++
CPPFLAGS  = -Wall -Weffc++

SRCDIR = ./
BINDIR = $(SRCDIR)bin/


rewrite:
	@echo 'Building target: rewrite'
	@echo 'Invoking: C++ Linker'
	mkdir -p $(BINDIR)
	$(CC) $(CPPFLAGS) rewrite.cpp -o $(BINDIR)rewrite
	@echo 'Finished building target: rewrite'
	@echo ' '

clean: 
	rm -f $(BINDIR)rewrite
	rm -d $(BINDIR)
