main: bin/main.o
	gcc -o ./bin/hexeditplus ./bin/main.o

bin/main.o: src/main.c
	gcc -g -Wall -ansi -c ./src/main.c -o ./bin/main.o

.PHONY: clean

clean:
	rm -f ./bin/*
