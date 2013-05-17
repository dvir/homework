# define Makefile variables
CC = gcc
CFLAGS  = -g -m32 
ASMC = nasm
ASMFLAGS = -g -f elf
LINKER = gcc

# Tool invocations
main: bin/add.o
	@echo 'Invoking Linker'
	$(LINKER) $(CFLAGS) -o bin/main src/main.c src/numbers.c bin/add.o
	@echo 'Finished building target: main'
	@echo ' '

bin/add.o: src/add.s
	$(ASMC) $(ASMFLAGS) -o bin/add.o src/add.s

.PHONY: clean

#Clean the build directory
clean: 
	rm -rf bin/*

