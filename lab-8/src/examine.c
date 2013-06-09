#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <elf.h>
#include <sys/stat.h>
#include <sys/mman.h>


int main(int argc, char *argv[]) {
    int fd; /* , num_of_section_headers; */
    void *map_start;   /* will point to the start of the memory mapped file */
    struct stat fd_stat;  /* this is needed to the size of the file */
    Elf32_Ehdr *header;  /* this will point to the header structure */

    if( (fd = open(argv[1], O_RDWR)) < 0 ) {
        perror("error in open");
        exit(-1);
    }

    if( fstat(fd, &fd_stat) != 0 ) {
        perror("stat failed");
        exit(-1);
    }

    if ( (map_start = mmap(0, fd_stat.st_size, PROT_READ | PROT_WRITE , MAP_SHARED, fd, 0)) <0 ) {
        perror("mmap failed");
        exit(-4);
    }

    header = (Elf32_Ehdr *) map_start;

    if (header->e_ident[0] != 0x7F
            || header->e_ident[1] != 0x45
            || header->e_ident[2] != 0x4C) {
        perror("Header isn't of ELF format.");
        exit(EXIT_FAILURE);
    }

    printf("Magic #: %X %X %X\n", header->e_ident[0], header->e_ident[1], header->e_ident[2]);
    printf("Data Encoding: %X\n", header->e_ident[5]);
    printf("Entry Point: 0x%x\n", header->e_entry);
    printf("Section Offset: 0x%x\n", (Elf32_Off) header->e_shoff);
    printf("Section Entries: %u\n", (Elf32_Half) header->e_shnum);
    printf("Section Entry Size: %u\n", (Elf32_Half) header->e_shentsize);
    printf("Program Offset: 0x%x\n", (Elf32_Off) header->e_phoff);
    printf("Program Entries: %u\n", (Elf32_Half) header->e_phnum);
    printf("Program Entry Size: %u\n", (Elf32_Half) header->e_phentsize);

    munmap(map_start, fd_stat.st_size);
    close(fd);

    return EXIT_SUCCESS;
}
