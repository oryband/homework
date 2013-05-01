#define DT_REG 8
#define O_DIRECTORY 00200000

struct linux_dirent {
    unsigned long  d_ino;     /* inode number */
    unsigned long  d_off;     /* offset to next dirent */
    unsigned short d_reclen;  /* length of this dirent */
    char           d_name[];  /* filename (null-terminated) */
    char           pad;       /* Zero padding byte */
    char           d_type;    /* File type (only since Linux 2.6.4; * offset is (d_reclen - 1)) */
};
