# define some Makefile variables for the compiler and compiler flags
# to use Makefile variables later in the Makefile: $()
CC = g++
CFLAGS  = -g -Wall -Weffc++

 # All Targets
all: main

 # Tool invocations
main: bin/matrixU.o bin/student.o bin/course.o
	@echo 'Building target: matrixU'
	@echo 'Invoking: C++ Linker'
	$(CC) -o bin/main bin/matrixU.o bin/student.o bin/course.o
	@echo 'Finished building target: main'
	@echo ' '

bin/matrixU.o: src/matrixU.cpp
	$(CC) $(FLAGS) -c -Linclude -o bin/matrixU.o src/matrixU.cpp

 # Depends on the source and header files
bin/student.o: src/student.cpp include/student.h
	$(CC) $(CFLAGS) -c -Linclude -o bin/student.o src/student.cpp

 # Depends on the source and header files 
bin/course.o: src/course.cpp include/course.h
	$(CC) $(CFLAGS) -c -Linclude -o bin/course.o src/course.cpp

 #Clean the build directory
clean: 
	rm -rf bin/*

