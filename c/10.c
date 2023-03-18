#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

#define MAX_INSTR 241
#define BUF_SIZE 20
#define WIDTH 40

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

    char buffer[BUF_SIZE];
    int val = 0;
    int x = 1;
    int cycles[MAX_INSTR] = {0};
    int curr_cycle = 1;

    while (fgets(buffer, BUF_SIZE, in) && curr_cycle < MAX_INSTR) {
        if (sscanf(buffer, "addx %d", &val)) {
            cycles[curr_cycle++] = x;
            cycles[curr_cycle++] = x;
            x += val;
        } else {
            cycles[curr_cycle++] = x;
        }
    }

    if (ferror(in)) {
        perror("Error when reading file");
        return EXIT_FAILURE;
    }

    // part 1
    printf("%d\n", cycles[20] * 20 + cycles[60] * 60 + cycles[100] * 100 +
            cycles[140] * 140 + cycles[180] * 180 + cycles[220] * 220);

    // part 2
    for (int c = 1; c < MAX_INSTR; c++) {
        int pix = (c - 1) % WIDTH;
        if (cycles[c]-1 == pix || cycles[c] == pix || cycles[c]+1 == pix) {
            printf("#");
        } else {
            printf(".");
        }
        if (c % WIDTH == 0) printf("\n");
    }

    fclose(in);

    return EXIT_SUCCESS;
}
