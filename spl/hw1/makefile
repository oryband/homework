CC = g++
CFLAGS  = -g -Wall -Weffc++

all: base2 print_words

base2:
	@echo 'Building target: base2'
	@echo 'Invoking: C++ Linker'
	mkdir -p bin/
	$(CC) base2.cpp -o bin/base2
	@echo 'Finished building target: base'
	@echo ' '

print_words:
	@echo 'Building target: print_words'
	@echo 'Invoking: C++ Linker'
	mkdir -p bin/
	$(CC) print_words.cpp -o bin/print_words
	@echo 'Finished building target: print_words'
	@echo ' '

clean: 
	rm -rf bin/*
