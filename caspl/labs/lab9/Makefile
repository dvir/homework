main: bin/main.o
	gcc -m32 -o ./bin/examine ./bin/main.o

bin/main.o: src/main.c
	gcc -m32 -g -Wall -ansi -c ./src/main.c -o ./bin/main.o

.PHONY: clean

clean:
	rm -f ./bin/*
