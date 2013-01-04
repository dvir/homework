# define some Makefile variables for the compiler and compiler flags
# to use Makefile variables later in the Makefile: $()
CC = g++
CFLAGS  = -g -Wall -Weffc++

 # All Targets
all: main

 # Tool invocations
main: bin/client 
	@echo 'Building target: Mini IRC Client'
	@echo 'Invoking: C++ Linker'
	$(CC) -o bin/client bin/client.o
	@echo 'Finished building target: main'
	@echo ' '

bin/client.o: src/client.cpp include/client.h
	$(CC) $(CFLAGS) -c -Linclude -o bin/client.o src/client.cpp

 #Clean the build directory
clean: 
	rm -rf bin/*

