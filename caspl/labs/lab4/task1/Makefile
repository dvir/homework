
main: bin/start.o bin/main.o bin/util.o
	ld -melf_i386 ./bin/start.o ./bin/main.o ./bin/util.o -o ./bin/task0

bin/start.o: src/start.s 
	nasm -g -f elf -o ./bin/start.o ./src/start.s

bin/main.o: src/main.c
	gcc -g -m32 -Wall -ansi -c -nostdlib -fno-stack-protector ./src/main.c -o ./bin/main.o

bin/util.o: src/util.c
	gcc -g -m32 -Wall -ansi -c -nostdlib -fno-stack-protector ./src/util.c -o ./bin/util.o

.PHONY: clean

clean:
	rm -f ./bin/*
