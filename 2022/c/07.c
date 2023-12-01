#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAXDIR 1000
#define BUF_SIZE 256

#define TOTAL_DISK_SPACE 70000000
#define UPDATE_SPACE 30000000

int main(int argc, char **argv) {
    if (argc != 2) {
        printf("Usage: %s FILENAME\n", argv[0]);
        return EXIT_FAILURE;
    }

    errno = 0;
    FILE *in = fopen(argv[1], "r");
    if (in == NULL) {
        perror("Failed to open file");
        return EXIT_FAILURE;
    }

    char input[BUF_SIZE] = {'\0'};
    long stack[MAXDIR] = {0};
    long dirsizes[MAXDIR] = {0};
    long cursize = 0;
    long isizes = 0, sp = 0;

    fgets(input, BUF_SIZE, in); // skip first line

    while (fgets(input, BUF_SIZE, in)) {
        if (sscanf(input, "%ld", &cursize)) {
            stack[sp] += cursize;
        } else if (strstr(input, "..")) {
            long size = stack[sp];
            stack[sp--] = 0;
            dirsizes[isizes++] = size;
            stack[sp] += size;
        } else if (strstr(input, "$ cd")) {
            sp++;
        }
    }

    while (sp > 0) {
        dirsizes[isizes++] = stack[sp];
        stack[sp-1] += stack[sp];
        sp--;
    }
    dirsizes[isizes] = stack[sp];

    // part 1
    long total = 0;
    for (long i = 0; i <= isizes; i++) {
        if (dirsizes[i] <= 100000)
            total += dirsizes[i];
    }
    printf("%ld\n", total);

    // part 2
    long space = UPDATE_SPACE - (TOTAL_DISK_SPACE - dirsizes[isizes]);
    long min = dirsizes[isizes];
    for (int i = 0; i <= isizes; i++) {
        if (dirsizes[i] >= space && dirsizes[i] < min)
            min = dirsizes[i];
    }
    printf("%ld\n", min);

    if (ferror(in)) {
        perror("Error when reading file");
        return EXIT_FAILURE;
    }

    fclose(in);

    return EXIT_SUCCESS;
}
