#include <stdio.h>
#include <stdlib.h>
 
/* forward declarations: */
int inc(int i); /* return the value of i + 1 */
int dec(int i); /* return the value of i - 1 */
int iprt(int i); /* print the value of i and return it unchanged */
struct int_array {
    int *array;
    int sz;
    void (*map) (struct int_array *, int (*f) (int));
};
void int_array_map(struct int_array *iarray, int (*f) (int));
void initialize_int_array(struct int_array *iarray, int *array, int sz);
void rec_map(int *array, int sz, int (*f) (int));

void task2c();

/* task 3 */
struct fun_desc {
    char *name;
    int (*fun)(int);
};

void task3();
 
int main(int argc, char **argv){
/*    
    task2c(); 
*/
    task3();

    return 0;
}

void printMenu(struct fun_desc *menu, int sz) {
    int i = 0;

    printf("+----MENU----+\n");
    while (i < sz) {
        printf("| %d) %s\n", i, menu[i].name);
        i++;
    }
    printf("| Else) Exit\n");
    printf("+------------+\n");
}

int getInt() {
    char input[256];
    fgets(input, 256, stdin);
    return atoi(input);
}

void task3() {
    int i;

    struct fun_desc menu[3] = {
        {"Increase", &inc},
        {"Decrease", &dec},
        {"Print", &iprt}
    };
    int choice;

    int array[10];
    struct int_array iarray;

    initialize_int_array(&iarray, array, 0);

    printf("Please enter array size (0 <= size <= 10)");
    iarray.sz = getInt();
    for (i = 0; i < iarray.sz; i++) {
        printf("Please enter array[%d]", i);
        array[i] = getInt();
    }
    
    while (1) {
        printMenu(menu, 3);
        printf("Choice: ");
        choice = getInt();
    
        if (choice > 3 || choice < 0) {
            break;
        }

        iarray.map(&iarray, menu[choice].fun);
    }

    printf("Bye!\n");
}

void task2c() {
    int array[3]={1,1,1};
    struct int_array iarray;
    initialize_int_array(&iarray, array, 3);
    iarray.map(&iarray, iprt);
    iarray.map(&iarray, inc);
    iarray.map(&iarray, iprt);
}

void int_array_map(struct int_array *iarray, int (*f) (int)){
    rec_map(iarray->array, iarray->sz, f);
}
 
void initialize_int_array(struct int_array *iarray, int *array, int sz){
    int *newArray = malloc(sizeof(int)*sz);
    int i;
    for (i = 0; i < sz; i++) {
        newArray[i] = array[i];
    }
    (*iarray).array = newArray;
    (*iarray).sz = sz;
    (*iarray).map = &int_array_map;
}
 
void rec_map(int *array, int sz, int (*f) (int)){
    if (sz == 0) {
        /* nothing else to do */
        return;
    }

    *array = f(*array);
    rec_map(array+1, sz-1, f);
} 

int inc(int i) {
    return i+1;
}

int dec(int i) {
    return i-1;
}

int iprt(int i) {
    printf("%d\n", i);
    return i;
}
