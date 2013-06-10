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
    printf("Entry point: %x\n", header->e_entry); 
    printf("Section header offset: %d\n", header->e_shoff); 
    printf("Amount of section headers: %d\n", header->e_shnum); 
    printf("Size of each section header entry: %d\n", header->e_shentsize); 
    printf("Program header offset: %d\n", header->e_phoff); 
    printf("Amount of program headers: %d\n", header->e_phnum); 
    printf("Size of each program header entry: %d\n", header->e_phentsize); 

    close(fd);
    printf("\n");
}

void sectionsDisplay(char* filename) { 
    int fd = open(filename, O_RDONLY);
    if (fd == -1) {
        printf("Failed opening file %s\n", filename);
        return;
    }

    char* addr;
    int i;
    Elf32_Ehdr* header;

    addr = mmap(NULL, 10000000, PROT_READ, MAP_PRIVATE, fd, 0);
    header = (Elf32_Ehdr*) addr;
    
    if ((header->e_ident[0] != 0x7f) || (header->e_ident[1] != 'E') ||
        (header->e_ident[2] != 'L') || (header->e_ident[3] != 'F')) {
        printf("%s is not an ELF file.\n", filename);
        close(fd);
    }

    Elf32_Shdr* shdr = (Elf32_Shdr*) (addr + header->e_shoff);
    Elf32_Shdr* shstr = shdr + (header->e_shstrndx);
    char* stringdata = (char*)(addr + shstr->sh_offset);
    printf("[%3s] %-20s %-20s %-20s %-20s\n", "idx", "name", "address", "offset", "size");
    for (i = 0; i < header->e_shnum; ++i) {
        printf("[%3d] %-20s %-20x %-20x %-20x\n", i, (stringdata + shdr[i].sh_name), shdr[i].sh_addr, shdr[i].sh_offset, shdr[i].sh_size);
    }
}
void symbolsDisplay(char* filename) { 
    int fd = open(filename, O_RDONLY);
    if (fd == -1) {
        printf("Failed opening file %s\n", filename);
        return;
    }

    char* addr;
    int i;
    Elf32_Ehdr* header;

    addr = mmap(NULL, 100000000, PROT_READ, MAP_PRIVATE, fd, 0);
    header = (Elf32_Ehdr*) addr;
    
    if ((header->e_ident[0] != 0x7f) || (header->e_ident[1] != 'E') ||
        (header->e_ident[2] != 'L') || (header->e_ident[3] != 'F')) {
        printf("%s is not an ELF file.\n", filename);
        close(fd);
    }

    Elf32_Shdr* shdr = (Elf32_Shdr*) (addr + header->e_shoff);
    Elf32_Shdr* shstr = shdr + (header->e_shstrndx);
    char* stringdata = (char*)(addr + shstr->sh_offset);

    Elf32_Shdr* symshdr;
    char* symstringdata;

    for (i = 0; i < header->e_shnum; ++i) {
        if (shdr[i].sh_type == SHT_DYNSYM || shdr[i].sh_type == SHT_SYMTAB) {
            symshdr = (shdr + i);
        }
        if (shdr[i].sh_type == SHT_STRTAB && strcmp((stringdata + shdr[i].sh_name), ".dynstr") == 0) {
            symstringdata = (char*)(addr + shdr[i].sh_offset);
        }
    }
    
    Elf32_Sym* syment = (Elf32_Sym*) (addr + symshdr->sh_offset);
    printf("[%3s] %-20s %-20s %-20s %-20s\n", "idx", "value", "section_index", "section_name", "symbol_name");
    for (i = 0; i < (symshdr->sh_size / sizeof(Elf32_Sym)); ++i) {
        printf("[%3d] %-20x %-20d %-20s %-20s\n", i, syment[i].st_value, syment[i].st_shndx, (stringdata + shdr[syment[i].st_shndx].sh_name), (symstringdata + syment[i].st_name));
    }
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
