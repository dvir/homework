# define some Makefile variables for the compiler and compiler flags
# to use Makefile variables later in the Makefile: $()
CC = g++
CFLAGS = -g -Wall #-Weffc++ 
CLIBS = -lncurses -lboost_system -lboost_thread -lpthread

# All Targets
all: main

# Tool invocations
main: bin/client.o bin/connectionHandler.o bin/user.o bin/channel.o bin/message.o bin/utils.o bin/ui.o bin/ircsocket.o include/window.h include/contentwindow.h include/listwindow.h include/inputwindow.h
	@echo 'Building target: Mini IRC Client'
	@echo 'Invoking: C++ Linker'
	$(CC) -o ./client bin/client.o bin/connectionHandler.o bin/user.o bin/channel.o bin/message.o bin/utils.o bin/ui.o bin/ircsocket.o $(CLIBS)
	@echo 'Finished building target: main'
	@echo ' '

bin/client.o: src/client.cpp bin/connectionHandler.o bin/message.o bin/ircsocket.o bin/ircsocket.o
	$(CC) $(CFLAGS) -c -Linclude -o bin/client.o src/client.cpp

bin/ircsocket.o: bin/user.o bin/message.o bin/connectionHandler.o bin/utils.o bin/ui.o src/ircsocket.cpp include/ircsocket.h
	$(CC) $(CFLAGS) -c -Linclude -o bin/ircsocket.o src/ircsocket.cpp

bin/ui.o: src/ui.cpp include/ui.h include/window.h include/contentwindow.h include/listwindow.h include/inputwindow.h
	$(CC) $(CFLAGS) -c -Linclude -o bin/ui.o src/ui.cpp

bin/connectionHandler.o: src/connectionHandler.cpp include/connectionHandler.h
	$(CC) $(CFLAGS) -c -Linclude -o bin/connectionHandler.o src/connectionHandler.cpp

bin/message.o: bin/user.o src/message.cpp include/message.h 
	$(CC) $(CFLAGS) -c -Linclude -o bin/message.o src/message.cpp

bin/user.o: src/user.cpp include/user.h 
	$(CC) $(CFLAGS) -c -Linclude -o bin/user.o src/user.cpp

bin/channel.o: src/channel.cpp include/channel.h 
	$(CC) $(CFLAGS) -c -Linclude -o bin/channel.o src/channel.cpp

bin/utils.o: src/utils.cpp include/utils.h 
	$(CC) $(CFLAGS) -c -Linclude -o bin/utils.o src/utils.cpp

#Clean the build directory
clean:
	rm -f ./client
	rm -rf bin/*

run: main
	./client

zip: clean
	rm -f client.zip
	zip -r client.zip .
