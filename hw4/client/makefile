SHELL = /bin/sh


.SUFFIXES:
.SUFFIXES: .cpp .o


SRCDIR = ./src/
INCLUDEDIR = ./include/
BINDIR = ./bin/

INCLUDES = -I$(INCLUDEDIR)
BOOST = -lboost_system-mt
LIBS = -L/usr/lib -L/usr/local/lib

CPPFLAGS = -Wall -Weffc++ -g $(INCLUDES)

CC = g++


all: bin $(BINDIR)ConnectionHandler.o $(BINDIR)ProtocolHandler.o $(BINDIR)run_client.o
	@echo -n 'Building target all: '
	$(CC) $(CPPFLAGS) $(LIBS) $(BOOST) -o $(BINDIR)run_client $(BINDIR)ConnectionHandler.o $(BINDIR)ProtocolHandler.o $(BINDIR)run_client.o


bin:
	@echo -n 'Creating bin/: '
	mkdir -p $(BINDIR)


$(BINDIR)run_client.o: bin $(SRCDIR)run_client.cpp
	@echo -n 'Building target run_client.o: '
	$(CC) $(CPPFLAGS) -c -o $(BINDIR)run_client.o $(SRCDIR)run_client.cpp


$(BINDIR)ConnectionHandler.o: bin $(SRCDIR)ConnectionHandler.cpp
	@echo -n 'Building target ConnectionHandler.o: '
	$(CC) $(CPPFLAGS) -c -o $(BINDIR)ConnectionHandler.o $(SRCDIR)ConnectionHandler.cpp

$(BINDIR)ProtocolHandler.o: bin $(SRCDIR)ProtocolHandler.cpp
	@echo -n 'Building target ProtocolHandler.o: '
	$(CC) $(CPPFLAGS) -c -o $(BINDIR)ProtocolHandler.o $(SRCDIR)ProtocolHandler.cpp


.PHONY: clean
clean:
	@echo -n 'Cleaning $(BINDIR): '
	rm -f $(BINDIR)*.o $(BINDIR)run_client
