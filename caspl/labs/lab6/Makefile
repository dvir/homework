main: bin/main.o bin/LineParser.o
	gcc -o ./bin/shell ./bin/LineParser.o ./bin/main.o

bin/main.o: src/main.c
	gcc -g -Wall -ansi -c ./src/main.c -o ./bin/main.o

bin/LineParser.o: src/LineParser.c
	gcc -g -Wall -ansi -c ./src/LineParser.c -o ./bin/LineParser.o

.PHONY: clean

clean:
	rm -f ./bin/*
