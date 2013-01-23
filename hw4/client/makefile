SHELL = /bin/sh


.SUFFIXES:
.SUFFIXES: .cpp .o


SRCDIR = ./src/
INCLUDEDIR = ./include/
BINDIR = ./bin/

INCLUDES = -I$(INCLUDEDIR)
#BOOST = -lboost_system-mt -lboost_thread-mt
BOOST = -lboost_system -lboost_thread
LIBS = -L/usr/local/lib -L/usr/local/include

CPPFLAGS = -Wall -Weffc++ -g $(INCLUDES)

CC = g++


all: bin $(BINDIR)ConnectionHandler.o $(BINDIR)client.o
	@echo -n 'Building target all: '
	$(CC) $(CPPFLAGS) -o $(BINDIR)client $(BINDIR)ConnectionHandler.o $(BINDIR)client.o $(LIBS) $(BOOST)


bin:
	@echo -n 'Creating bin/: '
	mkdir -p $(BINDIR)


$(BINDIR)client.o: bin $(SRCDIR)client.cpp
	@echo -n 'Building target client.o: '
	$(CC) $(CPPFLAGS) -c -o $(BINDIR)client.o $(SRCDIR)client.cpp


$(BINDIR)ConnectionHandler.o: bin $(SRCDIR)ConnectionHandler.cpp
	@echo -n 'Building target ConnectionHandler.o: '
	$(CC) $(CPPFLAGS) -c -o $(BINDIR)ConnectionHandler.o $(SRCDIR)ConnectionHandler.cpp



.PHONY: clean
clean:
	@echo -n 'Cleaning $(BINDIR): '
	rm -f $(BINDIR)*.o $(BINDIR)client
