#include <stdio.h>
#include <stdlib.h>
#include <string.h> 
#include <sys/mman.h>
#include <unistd.h>
#include <fcntl.h>
#include <elf.h>

int hasMain(char* addr) { 
    int i;
    Elf32_Ehdr* header = (Elf32_Ehdr*) addr;

    Elf32_Shdr* shdr = (Elf32_Shdr*) (addr + header->e_shoff);
    Elf32_Shdr* shstr = shdr + (header->e_shstrndx);
    char* stringdata = (char*)(addr + shstr->sh_offset);

    Elf32_Shdr* symshdr = NULL;
    char* symstringdata;

    for (i = 0; i < header->e_shnum; ++i) {
        if (shdr[i].sh_type == SHT_DYNSYM || shdr[i].sh_type == SHT_SYMTAB) {
            symshdr = (shdr + i);
        }
        if (shdr[i].sh_type == SHT_STRTAB && (strcmp((stringdata + shdr[i].sh_name), ".dynstr") == 0
                                              || strcmp((stringdata + shdr[i].sh_name), ".strtab") == 0)) 
        {
            symstringdata = (char*)(addr + shdr[i].sh_offset);
        }
    }
    
    Elf32_Sym* syment = (Elf32_Sym*) (addr + symshdr->sh_offset);
    for (i = 0; i < (symshdr->sh_size / sizeof(Elf32_Sym)); ++i) {
        if (strcmp((symstringdata + syment[i].st_name), "main") == 0) {
            return 1;
        }
    }

    return 0;
}

int hasSymbol(char* addr, char* sym_name) { 
    int i;
    Elf32_Ehdr* header = (Elf32_Ehdr*) addr;

    Elf32_Shdr* shdr = (Elf32_Shdr*) (addr + header->e_shoff);
    Elf32_Shdr* shstr = shdr + (header->e_shstrndx);
    char* stringdata = (char*)(addr + shstr->sh_offset);

    Elf32_Shdr* symshdr = NULL;
    char* symstringdata;

    for (i = 0; i < header->e_shnum; ++i) {
        if (shdr[i].sh_type == SHT_DYNSYM || shdr[i].sh_type == SHT_SYMTAB) {
            symshdr = (shdr + i);
        }
        if (shdr[i].sh_type == SHT_STRTAB && (strcmp((stringdata + shdr[i].sh_name), ".dynstr") == 0
                                              || strcmp((stringdata + shdr[i].sh_name), ".strtab") == 0)) 
        {
            symstringdata = (char*)(addr + shdr[i].sh_offset);
        }
    }
    
    Elf32_Sym* syment = (Elf32_Sym*) (addr + symshdr->sh_offset);
    for (i = 0; i < (symshdr->sh_size / sizeof(Elf32_Sym)); ++i) {
        if (strcmp((symstringdata + syment[i].st_name), sym_name) == 0) {
            return 1;
        }
    }

    return 0;
}

char* findDuplicateSymbol(char* addr, char* addr2) { 
    int i;
    Elf32_Ehdr* header = (Elf32_Ehdr*) addr;

    Elf32_Shdr* shdr = (Elf32_Shdr*) (addr + header->e_shoff);
    Elf32_Shdr* shstr = shdr + (header->e_shstrndx);
    char* stringdata = (char*)(addr + shstr->sh_offset);

    Elf32_Shdr* symshdr = NULL;
    char* symstringdata;

    for (i = 0; i < header->e_shnum; ++i) {
        if (shdr[i].sh_type == SHT_DYNSYM || shdr[i].sh_type == SHT_SYMTAB) {
            symshdr = (shdr + i);
        }
        if (shdr[i].sh_type == SHT_STRTAB && (strcmp((stringdata + shdr[i].sh_name), ".dynstr") == 0
                                              || strcmp((stringdata + shdr[i].sh_name), ".strtab") == 0)) 
        {
            symstringdata = (char*)(addr + shdr[i].sh_offset);
        }
    }
    
    Elf32_Sym* syment = (Elf32_Sym*) (addr + symshdr->sh_offset);
    for (i = 0; i < (symshdr->sh_size / sizeof(Elf32_Sym)); ++i) {
        char* symname = (symstringdata + syment[i].st_name);
        if (strlen(symname) > 0 && hasSymbol(addr2, (symstringdata + syment[i].st_name))) {
            return symname;
        }
    }

    return NULL;
}

char* findMissingSymbol(char* addr, char* addr2) { 
    int i;
    Elf32_Ehdr* header = (Elf32_Ehdr*) addr;

    Elf32_Shdr* shdr = (Elf32_Shdr*) (addr + header->e_shoff);
    Elf32_Shdr* shstr = shdr + (header->e_shstrndx);
    char* stringdata = (char*)(addr + shstr->sh_offset);

    Elf32_Shdr* symshdr = NULL;
    char* symstringdata;

    for (i = 0; i < header->e_shnum; ++i) {
        if (shdr[i].sh_type == SHT_DYNSYM || shdr[i].sh_type == SHT_SYMTAB) {
            symshdr = (shdr + i);
        }
        if (shdr[i].sh_type == SHT_STRTAB && (strcmp((stringdata + shdr[i].sh_name), ".dynstr") == 0
                                              || strcmp((stringdata + shdr[i].sh_name), ".strtab") == 0)) 
        {
            symstringdata = (char*)(addr + shdr[i].sh_offset);
        }
    }
    
    Elf32_Sym* syment = (Elf32_Sym*) (addr + symshdr->sh_offset);
    for (i = 0; i < (symshdr->sh_size / sizeof(Elf32_Sym)); ++i) {
        char* symname = (symstringdata + syment[i].st_name);
        if (strlen(symname) > 0 && syment[i].st_shndx == SHN_UNDEF && !hasSymbol(addr2, symname)) {
            return symname;
        }
    }

    return NULL;
}

int main(int argc, char **argv){
    if (argc < 3) {
        printf("Format: can_link file1 file2.\n");
        exit(1);
    }
   
    char* file1 = argv[1];
    char* file2 = argv[2];
    int fd1, fd2;
    Elf32_Ehdr* header;
    char* addr1;
    char* addr2;

    fd1 = open(file1, O_RDONLY);
    if (fd1 == -1) {
        printf("Failed opening file %s\n", file1);
        return -1;
    }
    addr1 = mmap(NULL, 100000000, PROT_READ, MAP_PRIVATE, fd1, 0);
    header = (Elf32_Ehdr*) addr1;
    if ((header->e_ident[0] != 0x7f) || (header->e_ident[1] != 'E') ||
        (header->e_ident[2] != 'L') || (header->e_ident[3] != 'F')) {
        printf("%s is not an ELF file.\n", file1);
        close(fd1);
        return -1;
    }

    fd2 = open(file2, O_RDONLY);
    if (fd2 == -1) {
        printf("Failed opening file %s\n", file2);
        return -1;
    }
    addr2 = mmap(NULL, 100000000, PROT_READ, MAP_PRIVATE, fd2, 0);
    header = (Elf32_Ehdr*) addr2;
    if ((header->e_ident[0] != 0x7f) || (header->e_ident[1] != 'E') ||
        (header->e_ident[2] != 'L') || (header->e_ident[3] != 'F')) {
        printf("%s is not an ELF file.\n", file2);
        close(fd1);
        close(fd2);
        return -1;
    }

    if (hasMain(addr1) || hasMain(addr2)) {
        printf("main check: PASSED\n");
    } else {
        printf("main check: FAILED\n");
        /*return -1;*/
    }

    char* dup_sym_name = findDuplicateSymbol(addr1, addr2);
    if (dup_sym_name != NULL) {
        printf("duplicate check: FAILED (%s)\n", dup_sym_name);
    } else {
        printf("duplicate check: PASSED\n");
    }

    char* missing_sym_name = findMissingSymbol(addr1, addr2);
    if (missing_sym_name == NULL) {
        missing_sym_name = findMissingSymbol(addr2, addr1);
        if (missing_sym_name != NULL) {
            printf("no missing symbol: FAILED (%s)\n", missing_sym_name);
        } else {
            printf("no missing symbol: PASSED\n");
        }
    } else {
        printf("no missing symbol: FAILED (%s)\n", missing_sym_name);
    }

    close(fd1);
    close(fd2);

    return 0;
}
