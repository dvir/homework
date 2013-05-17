#include <stdio.h>
#include <stdlib.h>

typedef struct link link;
 
struct link {
    link* next;
    int data;
};

/* Task 1a */

void list_print(link* list); /* Print the data of every link in list. Each data is followed by a newline character. */
int list_size(link* list); /* Return the number of elements in the list. */
link *list_append(link* list, int data); /* Add a new link with the given data to the end
                                             of the list and return a pointer to the list (i.e., the first link in the list).
                                              If list is null - create a new link and return a pointer to the link. */
void list_free(link* list); /* Free the memory allocated by the list. */

void task1a() {
    link *list = NULL;
    list = list_append(list, 1);
    list = list_append(list, 2);
    list = list_append(list, 3);
    printf("The list size is: %d\n", list_size(list));
    printf("The list content is:\n");
    list_print(list);
    list_free(list);

    /*
    The code above should print:

        The list size is: 3
        The list content is:
        1
        2
        3
    */
}

/* Task 1b */

int reduce(link *l, int (*f)(int, int), int iv);
int sum(int a, int b); /* Returns the sum of a and b */
int max(int a, int b); /* Returns the maximum of a and b */

void task1b() {
    link *list = NULL;
    list = list_append(list, 1);
    list = list_append(list, 2);
    list = list_append(list, 3);
    printf("The sum of all list values: %d\n", reduce(list,sum,0));
    printf("The max value of list links: %d\n", reduce(list,max,0));
    printf("the list content:\n");
    list_print(list);
    list_free(list);

    /*
    The code above should print:

        The sum of all list values: 6
        The max value of list links: 3
        the list content:
        1
        2
        3
    */
}

/* Task 1c */

link *map(link *l, int (*f)(int));
int odd(int a);    /* returns a if a is an odd number, 0 otherwise */
int gt5(int a);    /* returns a if a is greater than 5, 0 otherwise */
int square(int a); /* returns the square of a */

void task1c() {
    link *list = NULL;
    link *new_list = NULL;
    int res = 0;
     
    list = list_append(list, 1);
    list = list_append(list, 3);
    list = list_append(list, 8);
      
    new_list = map(list,odd);
    res = reduce(new_list, max, 0);
     
    printf("The maximal odd number in the list is: %d\n", res);
    printf("new_list content is:\n");
    list_print(new_list);
         
    list_free(list);
    list_free(new_list);

    /*
    Will print:

        The maximal odd number in the list is: 3
        new_list content is:
        1
        3
        0
    */
}

/* Task 1d */

void task1d(int argc, char** argv) {
    if (argc < 2) {
        printf("No filename given.\n");
        return;
    }

    FILE* stream = fopen(argv[1], "r"); 
    if (stream == NULL) {
        printf("File %s could not be found.\n", argv[1]);
        return;
    }

    char line[512];
    int num1, num2;
    link* list = NULL;

    while (NULL != fgets(line, 512, stream)) {
        sscanf(line, "%d %d", &num1, &num2);
        list = list_append(list, abs(num1 + num2));
    }

    fclose(stream);

    link* oddList = map(list, odd);
    link* oddAndGt5List = map(oddList, gt5);
    link* squareList = map(list, square);

    printf("List size: %d\n", list_size(list));
    printf("Max of odd & greater than 5: %d\n", reduce(oddAndGt5List, max, 0));
    printf("Sum of squares: %d\n", reduce(squareList, sum, 0));
    list_print(list);

    list_free(oddList);
    list_free(oddAndGt5List);
    list_free(squareList);
    list_free(list);
}

int main(int argc, char** argv) {
    /*
    task1a();
    task1b();
    task1c();
    */

    task1d(argc, argv);
    return 0;
}

/* Task 1c */
link* map(link *list, int (*f)(int)) {
    link* newList = NULL;
    while (list != NULL) {
        newList = list_append(newList, f(list->data));
        list = list->next;
    }

    return newList;
}

int odd(int a) {
    if (a & 1 == 1) {
        return a;
    }

    return 0;
}

int gt5(int a) {
    if (a > 5) {
        return a;
    }

    return 0;
}

int square(int a) {
    return a * a;
}

/* Task 1b */

int reduce(link *list, int (*f)(int, int), int iv) {
    if (list == NULL) {
       return iv;
    }

    int result = f(list->data, iv);
    if (list->next == NULL) {
        /* end of list */
        return result;
    }

    return reduce(list->next, f, result);
}

int sum(int a, int b) {
    return a + b;
}

int max(int a, int b) {
    if (a > b) {
        return a;
    }

    return b;
}

/* Task 1a */
void list_print(link* list) {
    while (list != NULL) {
        printf("%d\n", list->data);
        list = list->next;
    }
}

int list_size(link* list) {
    int size = 0;
    while (list != NULL) {
        ++size;
        list = list->next;
    }

    return size;
}

link* list_append(link *list, int data) {
    link* head = list;

    link* newLink = (link*) malloc(sizeof(link));
    if (newLink == NULL) {
        return NULL;
    }

    newLink->next = NULL;
    newLink->data = data;

    if (list == NULL) {
        return newLink;
    }

    /* not an empty list; find the last element and append to it */
    while (list->next != NULL) {
        list = list->next;
    }

    list->next = newLink;
    return head;
}

void list_free(link* list) {
    if (list == NULL) {
        return;
    }

    if (list->next != NULL) {
        list_free(list->next);
    }

    free(list);
}
