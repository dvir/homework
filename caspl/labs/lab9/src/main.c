#include <stdio.h>
#include <stdlib.h>
#include <string.h> 
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <elf.h>

int foreach_phdr(void *map_start, void (*func)(Elf32_Phdr *,int), int arg) {
    Elf32_Ehdr* header;
    header = (Elf32_Ehdr*) map_start;
    Elf32_Phdr* phdr = (Elf32_Phdr*) (map_start + header->e_phoff);
    int i;

    for (i = 0; i < header->e_phnum; ++i) {
        func(phdr+i, i);
    }
}

void print_phdr(Elf32_Phdr* phdr, int arg) {
    printf("Program header number %d at address %x\n", arg, phdr->p_vaddr);
}

  Elf32_Word	p_type;			/* Segment type */
  Elf32_Off	p_offset;		/* Segment file offset */
  Elf32_Addr	p_vaddr;		/* Segment virtual address */
  Elf32_Addr	p_paddr;		/* Segment physical address */
  Elf32_Word	p_filesz;		/* Segment size in file */
  Elf32_Word	p_memsz;		/* Segment size in memory */
  Elf32_Word	p_flags;		/* Segment flags */
  Elf32_Word	p_align;		/* Segment alignment */

char* ptype_name(Elf32_Word p_type) {
    switch (p_type) {
        case PT_NULL: return "NULL";
        case PT_LOAD: return "LOAD";
        case PT_DYNAMIC: return "DYNAMIC";
        case PT_INTERP: return "INTERP";
        case PT_NOTE: return "NOTE";
        case PT_SHLIB: return "SHLIB";
        case PT_PHDR: return "PHDR";
        case PT_TLS: return "TLS";
        case PT_NUM: return "NUM";
        case PT_LOOS: return "LOOS";
        case PT_GNU_EH_FRAME: return "GNU_EH_FRAME";
        case PT_GNU_STACK: return "GNU_STACK";
        case PT_GNU_RELRO: return "GNU_RELRO";
        case PT_HIOS: return "HIOS";
        case PT_LOPROC: return "LOPROC";
        case PT_HIPROC: return "HIPROC";
    }
}

void print_phdr_pretty(Elf32_Phdr* phdr, int arg) {
    char flags[4] = {' ', ' ', ' ', '\0'};
    if (phdr->p_flags & PF_R) {
        flags[0] = 'R';
    }
    if (phdr->p_flags & PF_W) {
        flags[1] = 'W';
    }
    if (phdr->p_flags & PF_X) {
        flags[2] = 'E';
    }
    printf("%-12s %#-12x %#-12x %#-12x %#-12x %#-12x %s %#-12x\n",
            ptype_name(phdr->p_type),
            phdr->p_offset,
            phdr->p_vaddr,
            phdr->p_paddr,
            phdr->p_filesz,
            phdr->p_memsz,
            flags,
            phdr->p_align);
}

void print_phdr_pretty_with_flags(Elf32_Phdr* phdr, int arg) {
    char flags[4] = {' ', ' ', ' ', '\0'};
    if (phdr->p_flags & PF_R) {
        flags[0] = 'R';
    }
    if (phdr->p_flags & PF_W) {
        flags[1] = 'W';
    }
    if (phdr->p_flags & PF_X) {
        flags[2] = 'E';
    }
    printf("%-12s %#-12x %#-12x %#-12x %#-12x %#-12x %-12s %#-12x\n",
            ptype_name(phdr->p_type),
            phdr->p_offset,
            phdr->p_vaddr,
            phdr->p_paddr,
            phdr->p_filesz,
            phdr->p_memsz,
            flags,
            phdr->p_align);

    if (phdr->p_type == PT_LOAD) {
        int prot_flags = 0, map_flags = MAP_PRIVATE | MAP_FIXED;
        if (phdr->p_flags & PF_R) {
            prot_flags = prot_flags | PROT_READ;
        }
        if (phdr->p_flags & PF_W) {
            prot_flags = prot_flags | PROT_WRITE;
        }
        if (phdr->p_flags & PF_X) {
            prot_flags = prot_flags | PROT_EXEC;
        }

        printf("mmap flags: prot: %d, map: %d\n", prot_flags, map_flags);
    }
}

void task0(void* addr);
void task1a(void* addr);
void task1b(void* addr);

int main(int argc, char **argv){
    if (argc < 2) {
        printf("No filename given.\n");
        exit(1);
    }

    char* filename = argv[1];
    int fd = open(filename, O_RDONLY);
    if (fd == -1) {
        printf("Failed opening file %s\n", filename);
        return -1;
    }

    struct stat fd_stat;
    void* addr;
    int i;
    Elf32_Ehdr* header;

    if (fstat(fd, &fd_stat) != 0) {
        perror("stat failed");
        return -1;
    }

    addr = mmap(NULL, fd_stat.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
    header = (Elf32_Ehdr*) addr;

    if ((header->e_ident[0] != 0x7f) || (header->e_ident[1] != 'E') ||
            (header->e_ident[2] != 'L') || (header->e_ident[3] != 'F')) {
        printf("%s is not an ELF file.\n", filename);
        close(fd);
        munmap(addr, fd_stat.st_size);
        return -1;
    }

    /*task0(addr);*/
    /*task1a(addr);*/
    task1b(addr);

    close(fd);
    munmap(addr, fd_stat.st_size);
    return 0;
}

void task0(void* addr) {
    foreach_phdr(addr, &print_phdr, 7);
}

void task1a(void* addr) {
    printf("%-12s %-12s %-12s %-12s %-12s %-12s %-12s %-12s\n", 
            "Type",
            "Offset",
            "VirtAddr",
            "PhysAddr",
            "FileSiz",
            "MemSiz",
            "Flg",
            "Align");
    foreach_phdr(addr, &print_phdr_pretty, 7);
}

void task1b(void* addr) {
    printf("%-12s %-12s %-12s %-12s %-12s %-12s %-12s %-12s\n", 
            "Type",
            "Offset",
            "VirtAddr",
            "PhysAddr",
            "FileSiz",
            "MemSiz",
            "Flg",
            "Align");
    foreach_phdr(addr, &print_phdr_pretty_with_flags, 7);
}

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

