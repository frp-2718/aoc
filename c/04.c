#include <stdbool.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

bool contained(int x1, int y1, int x2, int y2);
bool overlap(int x1, int y1, int x2, int y2);

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

    char input[20] = {0};
    int total_contained = 0;
    int total_overlap = 0;
    while (fgets(input, 20, in)) {
        int a, b, c, d;
        sscanf(input, "%d-%d,%d-%d", &a, &b, &c, &d);
        if (contained(a, b, c, d)) {
            total_contained++;
            total_overlap++;
            continue;
        }
        if (overlap(a, b, c, d)) {
            total_overlap++;
        }
    }

    if (ferror(in)) {
        perror("Error when reading file");
        return EXIT_FAILURE;
    }

    printf("%d\n%d\n", total_contained, total_overlap);

    fclose(in);

    return EXIT_SUCCESS;
}

bool contained(int x1, int y1, int x2, int y2) {
    return (x1 <= x2 && y1 >= y2) || (x2 <= x1 && y2 >= y1);
}

bool overlap(int x1, int y1, int x2, int y2) {
    return (x1 >= x2 && x1 <= y2) || (y1 >= x2 && y1 <= y2);
}
