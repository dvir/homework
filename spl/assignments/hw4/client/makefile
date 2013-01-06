# define some Makefile variables for the compiler and compiler flags
# to use Makefile variables later in the Makefile: $()
CC = g++
CFLAGS = -g -Wall -Weffc++
CLIBS = -lncurses

# All Targets
all: clean main

# Tool invocations
main: bin/client.o
	@echo 'Building target: Mini IRC Client'
	@echo 'Invoking: C++ Linker'
	$(CC) -o bin/client bin/client.o $(CLIBS)
	@echo 'Finished building target: main'
	@echo ' '

bin/client.o:
	$(CC) $(CFLAGS) -c -Linclude -o bin/client.o src/client.cpp

#Clean the build directory
clean: 
	rm -rf bin/*

run: main
	./bin/client

