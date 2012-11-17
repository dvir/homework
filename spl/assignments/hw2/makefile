# define some Makefile variables for the compiler and compiler flags
# to use Makefile variables later in the Makefile: $()
CC = g++
CFLAGS  = -g -Wall -Weffc++

 # All Targets
all: main

 # Tool invocations
main: bin/randomUniversity.o bin/student.o bin/course.o bin/csstudent.o bin/pgstudent.o bin/cscourse.o bin/pgcourse.o bin/electivecourse.o bin/imageloader.o bin/imageoperations.o
	@echo 'Building target: randomUniversity'
	@echo 'Invoking: C++ Linker'
	$(CC) -o bin/main bin/randomUniversity.o bin/student.o bin/csstudent.o bin/pgstudent.o bin/course.o bin/cscourse.o bin/pgcourse.o bin/electivecourse.o bin/imageloader.o bin/imageoperations.o /usr/local/lib/libopencv_highgui.so /usr/local/lib/libopencv_core.so /usr/local/lib/libopencv_imgproc.so -L/usr/local/lib/ -L/usr/lib -lopencv_core -lopencv_highgui -lopencv_imgproc
	@echo 'Finished building target: main'
	@echo ' '

bin/randomUniversity.o: bin/student.o bin/course.o bin/csstudent.o bin/pgstudent.o bin/cscourse.o bin/pgcourse.o bin/electivecourse.o src/randomUniversity.cpp
	$(CC) $(FLAGS) -c -Linclude -o bin/randomUniversity.o src/randomUniversity.cpp

 # Depends on the source and header files
bin/student.o: src/student.cpp include/student.h
	$(CC) $(CFLAGS) -c -Linclude -o bin/student.o src/student.cpp
bin/csstudent.o: bin/student.o bin/course.o src/csstudent.cpp include/csstudent.h
	$(CC) $(CFLAGS) -c -Linclude -o bin/csstudent.o src/csstudent.cpp
bin/pgstudent.o: bin/student.o bin/course.o src/pgstudent.cpp include/pgstudent.h
	$(CC) $(CFLAGS) -c -Linclude -o bin/pgstudent.o src/pgstudent.cpp

 # Depends on the source and header files 
bin/course.o: src/course.cpp include/course.h
	$(CC) $(CFLAGS) -c -Linclude -o bin/course.o src/course.cpp
bin/cscourse.o: bin/course.o bin/student.o src/cscourse.cpp include/cscourse.h
	$(CC) $(CFLAGS) -c -Linclude -o bin/cscourse.o src/cscourse.cpp
bin/pgcourse.o: bin/course.o bin/student.o src/pgcourse.cpp include/pgcourse.h
	$(CC) $(CFLAGS) -c -Linclude -o bin/pgcourse.o src/pgcourse.cpp
bin/electivecourse.o: bin/course.o bin/student.o src/electivecourse.cpp include/electivecourse.h
	$(CC) $(CFLAGS) -c -Linclude -o bin/electivecourse.o src/electivecourse.cpp

bin/imageloader.o: src/imageloader.cpp include/imageloader.h
	$(CC) $(CFLAGS) -c -I/usr/include/opencv -I/usr/include/ -o bin/imageloader.o src/imageloader.cpp `pkg-config opencv --cflags --libs`
		 
bin/imageoperations.o: src/imageoperations.cpp include/imageoperations.h
	$(CC) $(CFLAGS) -c -I/usr/include/opencv -I/usr/include/ -o bin/imageoperations.o src/imageoperations.cpp `pkg-config opencv --cflags --libs`

 #Clean the build directory
clean: 
	rm -rf bin/*

