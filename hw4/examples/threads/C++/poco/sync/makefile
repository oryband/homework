C_FLAGS:=-c -Wall -Weffc++ -g 
L_DFLAGS:=-L/usr/lib -lPocoUtil -lPocoFoundation

all: bin/threads.o
	g++ $(L_DFLAGS) bin/threads.o -o bin/Threads

bin/threads.o: src/threads.cpp
	g++ $(C_FLAGS) src/threads.cpp -o bin/threads.o

.PHONY: clean
clean:
	rm -f bin/*
