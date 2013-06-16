#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <unistd.h>
#include <fcntl.h>
#include <elf.h>
#include <sys/stat.h>
#include <sys/mman.h>

#define EXIT_FAILURE2 -2


int foreach_phdr(void *map_start, void (*func) (Elf32_Phdr *, int), int arg) {
    Elf32_Ehdr *header = (Elf32_Ehdr *) map_start;
    Elf32_Phdr *p_header = (Elf32_Phdr *) map_start + header->e_phoff;
    int i;

    for (i=0; i<header->e_phnum; i++) {
        func((Elf32_Phdr *) (p_header + i*header->e_phentsize), i);
    }
}

void message(Elf32_Phdr *header, int i) {
    printf("[%d]:\t0x%08x\n", i, header->p_vaddr);
}


int main(int argc, char *argv[]) {
    int fd;
    void *map_start;
    struct stat fd_stat;

    if ( (fd = open(argv[1], O_RDWR)) < 0 ) {
        perror("error in open()");
        exit(-1);
    }
    if( fstat(fd, &fd_stat) != 0 ) {
        perror("stat failed.");
        exit(-1);
    }
    if ( (map_start = mmap(0, fd_stat.st_size, PROT_READ | PROT_WRITE , MAP_SHARED, fd, 0)) < 0 ) {
        perror("mmap failed");
        exit(-4);
    }

    foreach_phdr(map_start, message, 0);

    munmap(map_start, fd_stat.st_size);
    close(fd);
    return EXIT_SUCCESS;
}
