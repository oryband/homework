SHELL = /bin/sh


.SUFFIXES:
.SUFFIXES: .cpp .o


SRCDIR = ./src/
INCLUDEDIR = ./include/
BINDIR = ./bin/

INCLUDES = -I$(INCLUDEDIR)
LIBS = -L/usr/lib -L/usr/local/lib

CC = g++
CPPFLAGS = -Wall -Weffc++ -g $(INCLUDES) $(LIBS)


all: bin $(BINDIR)ConnectionHandler.o $(BINDIR)ProtocolHandler.o $(BINDIR)Client.o $(BINDIR)run_client.o
	@echo -n 'Building target all: '
	$(CC) $(CPPFLAGS) -o $(BINDIR)run_client $(BINDIR)ConnectionHandler.o $(BINDIR)ProtocolHandler.o $(BINDIR)Client.o $(BINDIR)run_client.o


bin:
	@echo -n 'Creating bin/: '
	mkdir -p $(BINDIR)


$(BINDIR)run_client.o: bin $(SRCDIR)run_client.cpp
	@echo -n 'Building target run_client.o: '
	$(CC) $(CPPFLAGS) -c -o $(BINDIR)run_client.o $(SRCDIR)run_client.cpp


$(BINDIR)Client.o: bin $(SRCDIR)Client.cpp
	@echo -n 'Building target Client.o: '
	$(CC) $(CPPFLAGS) -c -o $(BINDIR)Client.o $(SRCDIR)Client.cpp

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
