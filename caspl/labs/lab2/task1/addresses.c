#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int addr5;
int addr6;

int foo();
void point_at(void *p);
void counter();

int main (int argc, char** argv){
	static int addr1;
	int addr2;
	int addr3;
	char* yos="ree";
	int * addr4 = (int*)(malloc(50));
	printf("%p\n",&addr1);
	printf("%p\n",&addr2);
	printf("%p\n",&addr3);
	printf("%p\n",foo);
	printf("%p\n",&addr5);
	point_at(&addr5);
	printf("%p\n",&addr6);
	printf("%p\n",yos);
	printf("%p\n",addr4);

    int iarray[3];
    char carray[3];

    printf("iarray0: %p\n", &iarray);
    printf("iarray1: %p\n", &iarray[1]);
    printf("iarray2: %p\n", &iarray[2]);
    printf("carray0: %p\n", &carray);
    printf("carray1: %p\n", &carray[1]);
    printf("carray2: %p\n", &carray[2]);
    
    printf("iarray+1: %p\n", &iarray+1);
    printf("iarray+2: %p\n", &iarray+2);
    printf("carray+1: %p\n", &carray+1);
    printf("carray+2: %p\n", &carray+2);

    counter();
    counter();
    counter();
    counter();
    counter();

    {
        int x;
        printf("%p\n", &x);
    }

    {
        int y;
        printf("%p\n", &y);
    }
	return 0;
}

void counter() {
    static int count = 1;
    printf("count: %d\n", count++);
}

int foo(){
	return -1;
}

void point_at(void *p){
	int local;
	long dist1 = (size_t)&addr6 - (size_t)p;
	long dist2 = (size_t)&local - (size_t)p;
	long dist3 = (size_t)&foo - (size_t)p;
	printf("dist1: %ld\n",dist1);
	printf("dist2: %ld\n",dist2);
	printf("dist3: %ld\n",dist3);
}
