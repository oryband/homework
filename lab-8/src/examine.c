#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <elf.h>
#include <sys/stat.h>
#include <sys/mman.h>


int main(int argc, char *argv[]) {
    int fd;
    unsigned int i, j;
    struct stat fd_stat;
    void *map_start;
    Elf32_Ehdr *header;
    Elf32_Shdr *sections;
    Elf32_Sym *symbol;

    if( (fd = open(argv[1], O_RDWR)) < 0 ) {
        perror("error in open");
        exit(-1);
    }

    if( fstat(fd, &fd_stat) != 0 ) {
        perror("stat failed");
        exit(-1);
    }

    if ( (map_start = mmap(0, fd_stat.st_size, PROT_READ | PROT_WRITE , MAP_SHARED, fd, 0)) < 0 ) {
        perror("mmap failed");
        exit(-4);
    }

    header = (Elf32_Ehdr *) map_start;
    sections = (Elf32_Shdr *) ((unsigned int) map_start + header->e_shoff);

    /* ELF Header */
    printf("Magic #: %X %X %X\n", header->e_ident[0], header->e_ident[1], header->e_ident[2]);
    if (header->e_ident[0] != 0x7F
            || header->e_ident[1] != 0x45
            || header->e_ident[2] != 0x4C) {
        perror("File isn't of ELF format.");
        exit(EXIT_FAILURE);
    }
    printf("Data Encoding: %X\n", header->e_ident[5]);
    printf("Entry Point: 0x%x\n", header->e_entry);
    printf("Section Offset: 0x%x\n", (Elf32_Off) header->e_shoff);
    printf("Section Entries: %u\n", (Elf32_Half) header->e_shnum);
    printf("Section Entry Size: %u\n", (Elf32_Half) header->e_shentsize);
    printf("Program Offset: 0x%x\n", (Elf32_Off) header->e_phoff);
    printf("Program Entries: %u\n", (Elf32_Half) header->e_phnum);
    printf("Program Entry Size: %u\n", (Elf32_Half) header->e_phentsize);

    /* Sections */
    printf("\nSections:\n");
    for (i=0; i < header->e_shnum; i++) {
        printf("[%d]\t%s\t0x%x\t0x%x\t%u\n",
                i,
                (char*) map_start + sections[header->e_shstrndx].sh_offset + sections[i].sh_name,
                sections[i].sh_type,
                sections[i].sh_offset,
                sections[i].sh_size);
    }

    printf("\nSymbols:\n");
    for (i=0; i < header->e_shnum; i++){
        if (sections[i].sh_type == 2 || sections[i].sh_type == 11) {
            symbol = (Elf32_Sym*) ((char*) map_start + sections[i].sh_offset);
            for(j=0; j < sections[i].sh_size / sizeof(Elf32_Sym); j++) {
                printf("[%d]\t0x%x\t%u\t%s\t%s\n",
                        j,
                        symbol[j].st_value,
                        symbol[j].st_shndx,
                        (char*) map_start + sections[header->e_shstrndx].sh_offset + sections[i].sh_name,
                        (char*) map_start + sections[sections[i].sh_link].sh_offset + symbol[j].st_name);
            }
        }
    }

    munmap(map_start, fd_stat.st_size);
    close(fd);

    return EXIT_SUCCESS;
}
