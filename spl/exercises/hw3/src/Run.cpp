
#include "../include/LinkedList.h"

#include <string>


int main(int argc, char *argv[]) {
    Link* link = new Link("test", 0);

    Link* linkCopy = new Link(*link);

    delete linkCopy;
    
    List* list = new List();

    List* listCopy = new List(*list);

    *listCopy = *list;

    delete list;
    
	return 0;
}




