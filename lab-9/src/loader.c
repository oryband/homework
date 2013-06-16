#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <elf.h>
#include <sys/stat.h>
#include <sys/mman.h>


int startup(int argc, char **argv, void (*start) ());


int foreach_phdr(void *map_start, void (*func) (Elf32_Phdr*, int), int arg) {
    Elf32_Ehdr *header = (Elf32_Ehdr*) map_start;
    Elf32_Phdr *p_header = (Elf32_Phdr*) (map_start + header->e_phoff);
    int i;

    for (i=0; i<header->e_phnum; i++) {
        func((Elf32_Phdr*) ( ((void*) p_header) + i*header->e_phentsize ), arg);
    }

    return EXIT_SUCCESS;
}

int convert(int b) {
    int r = 0;
    r |= (b & PF_R ? PROT_READ  : 0);
    r |= (b & PF_W ? PROT_WRITE : 0);
    r |= (b & PF_X ? PROT_EXEC  : 0);
    return r;
}

void message(Elf32_Phdr *p_header, int i) {
    switch(p_header->p_type) {
        case PT_NULL:         printf("%-15s", " NULL"); break;
        case PT_LOAD:         printf("%-15s", " LOAD"); break;
        case PT_DYNAMIC:      printf("%-15s", " DYNAMIC"); break;
        case PT_INTERP:       printf("%-15s", " INTERP"); break;
        case PT_NOTE:         printf("%-15s", " NOTE"); break;
        case PT_SHLIB:        printf("%-15s", " SHLIB"); break;
        case PT_PHDR:         printf("%-15s", " PHDR"); break;
        case PT_TLS:          printf("%-15s", " TLS"); break;
        case PT_NUM:          printf("%-15s", " NUM"); break;
        case PT_LOOS:         printf("%-15s", " LOOS"); break;
        case PT_GNU_EH_FRAME: printf("%-15s", " GNU_EH_FRAME"); break;
        case PT_GNU_STACK:    printf("%-15s", " GNU_STACK "); break;
        case PT_GNU_RELRO:    printf("%-15s", " GNU_RELRO"); break;
        case PT_LOSUNW:       printf("%-15s", " LOSUNW"); break;
        /* case PT_SUNWBSS:      printf("%-15s", " SUNWBSS"); break; */
        case PT_SUNWSTACK:    printf("%-15s", " SUNWSTACK"); break;
        case PT_HISUNW:       printf("%-15s", " HISUNW"); break;
        /* case PT_HIOS:         printf("%-15s", " HIOS"); break; */
        case PT_LOPROC:       printf("%-15s", " LOPROC"); break;
        case PT_HIPROC:       printf("%-15s", " HIPROC"); break;
        default: printf("%-15s", " ");
    }

    printf("0x%08x 0x%08x 0x%08x 0x%08x 0x%08x ",
            p_header->p_offset,
            p_header->p_vaddr,
            p_header->p_paddr,
            p_header->p_filesz,
            p_header->p_memsz);

    printf("%s", p_header->p_flags & PF_R ? "R" : " ");
    printf("%s", p_header->p_flags & PF_W ? "W" : " ");
    printf("%s", p_header->p_flags & PF_X ? "E" : " ");
    printf(" %d", convert(p_header->p_flags));

    printf(" %08x\n", p_header->p_align);
}

void load_phdr(Elf32_Phdr *phdr, int fd) {
    void *map_start;

    if (phdr->p_type == PT_LOAD) {
        if ( (map_start = mmap(
                        phdr->p_vaddr & 0xfffff000,
                        phdr->p_memsz + phdr->p_vaddr & 0xfff,
                        convert(phdr->p_flags),
                        MAP_PRIVATE | MAP_FIXED,
                        fd,
                        phdr->p_offset & 0xfffff000)) < 0 ) {

            perror("mmap failed");
            exit(-4);
        }
    }
}


int main(int argc, char *argv[]) {
    int fd;
    void *map_start;
    struct stat fd_stat;

    if ( (fd = open(argv[1], 0)) < 0 ) {
        perror("error in open()");
        exit(EXIT_FAILURE);
    }
    if( fstat(fd, &fd_stat) != 0 ) {
        perror("stat failed.");
        exit(EXIT_FAILURE);
    }
    if ( (map_start = mmap(0, fd_stat.st_size, PROT_READ, MAP_PRIVATE, fd, 0)) < 0 ) {
        perror("mmap failed");
        exit(-4);
    }

    /* foreach_phdr(map_start, message, 0); */
    foreach_phdr(map_start, load_phdr, fd);
    startup(argc -1, &argv[1], (void*) ((Elf32_Ehdr*) map_start)->e_entry);

    /* munmap(map_start, fd_stat.st_size); */
    /* close(fd); */
    return EXIT_SUCCESS;
}
