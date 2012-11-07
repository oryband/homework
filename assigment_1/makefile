SHELL = /bin/sh

.SUFFIXES:
.SUFFIXES: .cpp .o

SRC = ./src/
INCLUDE = ./include/
BIN = ./bin/

CC=g++
CPPFLAGS=-Wall -Weffc++ -O -g -Linclude -I $(INCLUDE)



all: $(BIN)matrixU.o $(BIN)Uni.o $(BIN)Course.o $(BIN)Student.o
	@echo 'Building target: all'
	@echo 'Invoking: C++ Linker'
	mkdir -p $(BIN)
	$(CC) $(CPPFLAGS) -o $(BIN)main $(BIN)matrixU.o $(BIN)Uni.o $(BIN)Course.o $(BIN)Student.o
	@echo 'Finished building target: main'
	@echo ' '


$(BIN)matrixU.o: $(BIN)Uni.o $(BIN)Course.o $(BIN)Student.o
	mkdir -p $(BIN)
	$(CC) $(CPPFLAGS) -c -o $(BIN)matrixU.o $(BIN)Uni.o $(BIN)Course.o $(BIN)Student.o $(SRC)matrixU.cpp
	@echo ' '

$(BIN)Uni.o: $(BIN)Course.o $(BIN)Student.o
	mkdir -p $(BIN)
	$(CC) $(CPPFLAGS) -c -o $(BIN)Uni.o $(BIN)Course.o $(BIN)Student.o $(SRC)Uni.cpp $(INCLUDE)Uni.h
	@echo ' '

$(BIN)Course.o: $(BIN)Student.o
	mkdir -p $(BIN)
	$(CC) $(CPPFLAGS) -c -o $(BIN)Course.o $(BIN)Student.o $(SRC)Course.cpp $(INCLUDE)Course.h
	@echo ' '

$(BIN)Student.o:
	mkdir -p $(BIN)
	$(CC) $(CPPFLAGS) -c -o $(BIN)Student.o $(SRC)Student.cpp $(INCLUDE)Student.h
	@echo ' '



clean: 
	#rm -f $(BIN)matrixU.o
	rm -rf $(BIN)
