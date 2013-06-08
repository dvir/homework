#include <stdio.h>
#include <stdlib.h>
#include <string.h> 

int is_pal (char* str) {
    int length = strlen(str);
    int i;
    for (i = 0; i < length / 2; ++i) {
        if (str[i] != str[length - i - 1]) {
            return 0;
        }
    }

    return 1;
}

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
struct fun_desc {
    char *name;
    void (*fun)(char*);
};
void printMenu(struct fun_desc *menu, int sz);
int getInt();
void memDisplay(char* filename) {
    char* addr;
    unsigned int length;
    int i;
    char buf[100];
    
    printf("Enter <Address> <Length>:\n");
    fgets(buf, 100, stdin);
    sscanf(buf, "%p %d", &addr, &length);

    for (i = 0; i < length; ++i) {
        printf("%X ", addr[i]);
    }

}

void fileDisplay(char* filename) {
    FILE* file = fopen(filename, "r");
    if (file == NULL) {
        printf("Failed opening file %s\n", filename);
        return;
    }

    char* addr;
    unsigned int length;
    int i;
    char buf[100];
    
    printf("Enter <Address> <Length>:\n");
    fgets(buf, 100, stdin);
    sscanf(buf, "%p %d", &addr, &length);

    fseek(file, (size_t)addr, SEEK_SET);
    fread(buf, 1, length, file); 
    fclose(file);
    for (i = 0; i < length; ++i) {
        printf("%X ", buf[i]);
    }
    printf("\n");
}

void fileModify(char* filename) {
    FILE* file = fopen(filename, "r+");
    if (file == NULL) {
        printf("Failed opening file %s\n", filename);
        return;
    }

    char* addr;
    int val;
    char buf[100];
    
    printf("Enter <Address> <val>:\n");
    fgets(buf, 100, stdin);
    sscanf(buf, "%p %x", &addr, &val);

    printf("addy: %p, val: %x\n", addr, val);
    fseek(file, (size_t)addr, SEEK_SET);
    fprintf(file, "%c", val);
    fclose(file);
    printf("\n");
}

void fileCopy(char* filename) {
    FILE* file = fopen(filename, "r+");
    if (file == NULL) {
        printf("Failed opening file %s\n", filename);
        return;
    }

    char* target_loc;
    char* src_loc;
    char src_filename[100];
    unsigned int length;
    char buf[100];
    
    FILE* src_file;
    printf("Please enter <source-file> <s-location> <t-location> <length>:\n");
    fgets(buf, 100, stdin);
    sscanf(buf, "%s %p %p %d", src_filename, &src_loc, &target_loc, &length);
    src_file = fopen(src_filename, "r");
    if (src_file == NULL) {
        fclose(file);
        printf("Failed opening file %s for reading\n", src_filename);
        return;
    }
    
    fseek(src_file, (size_t)src_loc, SEEK_SET);
    fread(buf, 1, length, src_file);

    fseek(file, (size_t)target_loc, SEEK_SET);
    fwrite(buf, 1, length, file);

    fclose(file);
    fclose(src_file);
    printf("\n");
}

int main(int argc, char **argv){
    if (argc < 2) {
        printf("No filename given.\n");
        exit(1);
    }

    char* filename = argv[1];
    int choice;
    int menuSize = 4;
    struct fun_desc menu[] = {
        {"Mem Display", &memDisplay},
        {"File Display", &fileDisplay},
        {"File Modify", &fileModify},
        {"File Copy", &fileCopy}
    };

    printf("File: %s\n", filename);

    while (1) {
        printMenu(menu, menuSize);
        printf("Choice: ");
        choice = getInt();
    
        if (choice > 3 || choice < 0) {
            break;
        }

        (menu[choice].fun)(filename);
    }

    printf("Bye!\n");
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
