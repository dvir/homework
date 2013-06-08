#include <stdio.h>
#include <stdlib.h>
#include <string.h> 
#include <sys/mman.h>
#include <unistd.h>
#include <fcntl.h>
#include <elf.h>

struct fun_desc {
    char *name;
    void (*fun)(char*);
};
void printMenu(struct fun_desc *menu, int sz);
int getInt();

void headerDisplay(char* filename) {
    int fd = open(filename, O_RDONLY);
    if (fd == -1) {
        printf("Failed opening file %s\n", filename);
        return;
    }

    char* addr;
    char entry[4];
    unsigned int length;
    int i;
    char buf[1024];
    Elf32_Ehdr* header;

    addr = mmap(NULL, 8192, PROT_READ, MAP_PRIVATE, fd, 0);
    header = (Elf32_Ehdr*) addr;
    
    if ((header->e_ident[0] != 0x7f) || (header->e_ident[1] != 'E') ||
        (header->e_ident[2] != 'L') || (header->e_ident[3] != 'F')) {
        printf("%s is not an ELF file.\n", filename);
        close(fd);
    }

    printf("Magic: %.3s\n", header->e_ident+1); 
    printf("Data encoding: ");
    switch (header->e_type) {
        case ELFDATANONE:
            printf("Invalid");
            break;

        case ELFDATA2LSB:
            printf("2's complement, little endian");
            break;

        case ELFDATA2MSB:
            printf("2's complement, big endian");
            break;
    }
    printf("\n");
    printf("Entry point: %p\n", header->e_entry); 
    printf("Section header offset: %d\n", header->e_shoff); 
    printf("Amount of section headers: %d\n", header->e_shnum); 
    printf("Size of each section header entry: %d\n", header->e_shentsize); 
    printf("Program header offset: %d\n", header->e_phoff); 
    printf("Amount of program headers: %d\n", header->e_phnum); 
    printf("Size of each program header entry: %d\n", header->e_phentsize); 

    close(fd);
    printf("\n");
}

void sectionsDisplay(char* filename) { }
void symbolsDisplay(char* filename) { }

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
    int menuSize = 3;
    struct fun_desc menu[] = {
        {"Header", &headerDisplay},
        {"Sections", &sectionsDisplay},
        {"Symbols", &symbolsDisplay},
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
