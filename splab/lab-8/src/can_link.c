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


int main(int argc, char *argv[]) {
    int fd1, fd2;
    unsigned int i,  j,
                 i2, j2,
                 sections_count,
                 sections_count2;
    Elf32_Section symbol_section,
                  symbol_section2;
    char *symbol_name,
         *symbol_name2;
    bool main_found = false,
         duplicate_symbols = false,
         missing_symbols = false;
    struct stat fd_stat;
    void *map_start1, *map_start2;
    Elf32_Ehdr *header1, *header2;
    Elf32_Shdr *sections1, *sections2;
    Elf32_Sym *symbol, *symbol2;

    if( (fd1 = open(argv[1], O_RDWR)) < 0 ) {
        perror("error in open");
        exit(-1);
    }
    if( (fd2 = open(argv[2], O_RDWR)) < 0 ) {
        perror("error in open");
        exit(-1);
    }

    if( fstat(fd1, &fd_stat) != 0 ) {
        perror("stat failed");
        exit(-1);
    }
    if( fstat(fd2, &fd_stat) != 0 ) {
        perror("stat failed");
        exit(-1);
    }

    if ( (map_start1 = mmap(0, fd_stat.st_size, PROT_READ | PROT_WRITE , MAP_SHARED, fd1, 0)) < 0 ) {
        perror("mmap failed");
        exit(-4);
    }
    if ( (map_start2 = mmap(0, fd_stat.st_size, PROT_READ | PROT_WRITE , MAP_SHARED, fd2, 0)) < 0 ) {
        perror("mmap failed");
        exit(-4);
    }

    header1 = (Elf32_Ehdr *) map_start1;
    header2 = (Elf32_Ehdr *) map_start2;
    sections1 = (Elf32_Shdr *) ((unsigned int) map_start1 + header1->e_shoff);
    sections2 = (Elf32_Shdr *) ((unsigned int) map_start2 + header2->e_shoff);

    if (header1->e_ident[0] != 0x7F
            || header1->e_ident[1] != 0x45
            || header1->e_ident[2] != 0x4C) {
        perror("File isn't of ELF format.");
        exit(EXIT_FAILURE2);
    }
    if (header2->e_ident[0] != 0x7F
            || header2->e_ident[1] != 0x45
            || header2->e_ident[2] != 0x4C) {
        perror("File isn't of ELF format.");
        exit(EXIT_FAILURE2);
    }

    /* Search for main. */
    for (i=0; i < header1->e_shnum && ! main_found; i++){
        if (sections1[i].sh_type == SHT_SYMTAB || sections1[i].sh_type == SHT_DYNSYM) {
            symbol = (Elf32_Sym*) ((char*) map_start1 + sections1[i].sh_offset);
            sections_count = sections1[i].sh_size / sizeof(Elf32_Sym);

            for(j=0; j < sections_count; j++) {
                symbol_name = (char*) map_start1 + sections1[sections1[i].sh_link].sh_offset + symbol[j].st_name;
                if (strcmp(symbol_name, "main") == 0) {
                    main_found = true;
                    break;
                }
            }
        }
    }
    for (i=0; i < header2->e_shnum && ! main_found; i++){
        if (sections2[i].sh_type == SHT_SYMTAB || sections2[i].sh_type == SHT_DYNSYM) {
            symbol = (Elf32_Sym*) ((char*) map_start2 + sections2[i].sh_offset);
            sections_count = sections2[i].sh_size / sizeof(Elf32_Sym);

            for(j=0; j < sections_count; j++) {
                symbol_name = (char*) map_start2 + sections2[sections2[i].sh_link].sh_offset + symbol[j].st_name;
                if (strcmp(symbol_name, "main") == 0) {
                    main_found = true;
                    break;
                }
            }
        }
    }

    /* Search for duplicate symbols. */
    for (i=0; i < header1->e_shnum && ! duplicate_symbols; i++){
        if (sections1[i].sh_type == SHT_SYMTAB || sections1[i].sh_type == SHT_DYNSYM) {
            symbol = (Elf32_Sym*) ((char*) map_start1 + sections1[i].sh_offset);
            sections_count = sections1[i].sh_size / sizeof(Elf32_Sym);

            for(j=0; j < sections_count && ! duplicate_symbols; j++) {
                symbol_name = (char*) map_start1 + sections1[sections1[i].sh_link].sh_offset + symbol[j].st_name;

                /* compare all symbols (from 2nd file) to this symbol (from 1st file). */
                for (i2=0; i2 < header2->e_shnum && ! duplicate_symbols; i2++){
                    if (sections2[i2].sh_type == SHT_SYMTAB || sections2[i2].sh_type == SHT_DYNSYM) {
                        symbol2 = (Elf32_Sym*) ((char*) map_start2 + sections2[i2].sh_offset);
                        sections_count2 = sections2[i2].sh_size / sizeof(Elf32_Sym);

                        for(j2=0; j2 < sections_count2 && ! duplicate_symbols; j2++) {
                            symbol_name2 = (char*) map_start2 + sections2[sections2[i2].sh_link].sh_offset + symbol2[j2].st_name;
                            if (strcmp(symbol_name, symbol_name2) == 0) {
                                duplicate_symbols = true;
                                break;
                            }
                        }
                    }
                }
            }
        }
    }

    /* Search for missing symbols. */
    for (i=0; i < header1->e_shnum && ! missing_symbols; i++){
        if (sections1[i].sh_type == SHT_SYMTAB || sections1[i].sh_type == SHT_DYNSYM) {
            symbol = (Elf32_Sym*) ((char*) map_start1 + sections1[i].sh_offset);
            sections_count = sections1[i].sh_size / sizeof(Elf32_Sym);

            for(j=0; j < sections_count && ! missing_symbols; j++) {
                symbol_name = (char*) map_start1 + sections1[sections1[i].sh_link].sh_offset + symbol[j].st_name;
                symbol_section = symbol[j].st_shndx;
                if (symbol_section == SHN_UNDEF) {
                    missing_symbols = true;

                    for (i2=0; i2 < header2->e_shnum && missing_symbols; i2++){
                        if (sections2[i2].sh_type == SHT_SYMTAB || sections2[i2].sh_type == SHT_DYNSYM) {
                            symbol2 = (Elf32_Sym*) ((char*) map_start2 + sections2[i2].sh_offset);
                            sections_count2 = sections2[i2].sh_size / sizeof(Elf32_Sym);

                            for(j2=0; j2 < sections_count2 && missing_symbols; j2++) {
                                symbol_name2 = (char*) map_start2 + sections2[sections2[i2].sh_link].sh_offset + symbol2[j2].st_name;
                                symbol_section2 = symbol[j2].st_shndx;
                                if (symbol_section == symbol_section2
                                        && strcmp(symbol_name, symbol_name2) == 0) {

                                    missing_symbols = false;
                                    break;
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    /* Now check for missing symbols in opposite direction. */
    for (i2=0; i2 < header2->e_shnum && ! missing_symbols; i2++){
        if (sections2[i2].sh_type == SHT_SYMTAB || sections2[i2].sh_type == SHT_DYNSYM) {
            symbol2 = (Elf32_Sym*) ((char*) map_start2 + sections2[i2].sh_offset);
            sections_count2 = sections2[i2].sh_size / sizeof(Elf32_Sym);

            for(j2=0; j2 < sections_count2 && ! missing_symbols; j2++) {
                symbol_name2 = (char*) map_start2 + sections2[sections2[i2].sh_link].sh_offset + symbol2[j2].st_name;
                symbol_section2 = symbol[j2].st_shndx;
                if (symbol_section2 == SHN_UNDEF) {
                    missing_symbols = true;

                    for (i=0; i < header1->e_shnum && missing_symbols; i++){
                        if (sections1[i].sh_type == SHT_SYMTAB || sections1[i].sh_type == SHT_DYNSYM) {
                            symbol = (Elf32_Sym*) ((char*) map_start1 + sections1[i].sh_offset);
                            sections_count = sections1[i].sh_size / sizeof(Elf32_Sym);

                            for(j=0; j < sections_count && missing_symbols; j++) {
                                symbol_name = (char*) map_start1 + sections1[sections1[i].sh_link].sh_offset + symbol[j].st_name;
                                symbol_section = symbol[j].st_shndx;

                                if (symbol_section == symbol_section2
                                        && strcmp(symbol_name, symbol_name2) == 0) {

                                    missing_symbols = false;
                                    break;
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    if ( ! main_found) {
        perror("main check: FAILED\n");
        exit(EXIT_FAILURE);
    } else {
        printf("main check: PASSED\n");
    }
    if ( ! duplicate_symbols) {
        perror("duplicate check: FAILED\n");
        exit(EXIT_FAILURE);
    } else {
        printf("duplicate check: PASSED\n");
    }
    if (missing_symbols) {
        perror("missing_symbols: FAILED\n");
        exit(EXIT_FAILURE);
    } else {
        printf("missing_symbols: PASSED\n");
    }

    munmap(map_start1, fd_stat.st_size);
    munmap(map_start2, fd_stat.st_size);
    close(fd1);
    close(fd2);

    return EXIT_SUCCESS;
}
