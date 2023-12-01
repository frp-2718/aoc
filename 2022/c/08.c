#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "utils/input.h"

void set_visibles(size_t height, size_t width, char **input, bool visibles[height][width]);
int score(char **input, int row, int col, size_t height, size_t width);

int main(int argc, char **argv) {
    if (argc != 2) {
        printf("Usage: %s FILENAME\n", argv[0]);
        return EXIT_FAILURE;
    }

    char **input = file_to_lines(argv[1]);
    if (!input) {
        perror("Error reading file");
        return EXIT_FAILURE;
    }

    size_t width = strlen(input[0]) - 1;
    char **end = input;
    while (*++end) ;
    size_t height = (size_t)(end - input);

    // part 1
    bool visibles[height][width];
    for (size_t i = 0; i < height; i++) {
        for (size_t j = 0; j < width; j++) {
            visibles[i][j] = false;
        }
    }

    set_visibles(height, width, input, visibles);

    int total = 0;
    for (size_t i = 0; i < height; i++) {
        for (size_t j = 0; j < width; j++) {
            if (visibles[i][j]) total++;
        }
    }

    printf("%d\n", total);

    // part 2
    int max = 0;
    for (size_t i = 0; i < height; i++) {
        for (size_t j = 0; j < width; j++) {
            int scenic = score(input, i, j, height, width);
            if (scenic > max)
                max = scenic;
        }
    }

    printf("%d\n", max);

    free_lines(input);

    return EXIT_SUCCESS;
}

void set_visibles(size_t height, size_t width, char **input, bool visibles[height][width]) {
    int current_max_left, current_max_right;
    for (size_t i = 0; i < height; i++) {
        current_max_left = input[i][0];
        current_max_right = input[i][width-1];
        for (size_t j = 0, k = width-1; j < width; j++, k--) {
            if (j == 0 || j == width-1 || k == 0 || k == width-1) {
                visibles[i][j] = true;
                continue;
            }
            if (input[i][j] > current_max_left) {
                visibles[i][j] = true;
                current_max_left = input[i][j];
            }
            if (input[i][k] > current_max_right) {
                visibles[i][k] = true;
                current_max_right = input[i][k];
            }
        }
    }

    int current_max_top, current_max_bottom;
    for (size_t j = 0; j < width; j++) {
        current_max_top = input[0][j];
        current_max_bottom = input[width-1][j];
        for (size_t i = 0, k = height-1; i < height; i++, k--) {
            if (i == 0 || j == height-1 || k == 0 || k == height-1) {
                visibles[i][j] = true;
                continue;
            }
            if (input[i][j] > current_max_top) {
                visibles[i][j] = true;
                current_max_top = input[i][j];
            }
            if (input[k][j] > current_max_bottom) {
                visibles[k][j] = true;
                current_max_bottom = input[k][j];
            }
        }
    }
}

int score(char **input, int row, int col, size_t height, size_t width) {
    int res[4] = {0};

    for (int i = row-1; i >= 0; i--) {
        res[0] += 1;
        if (input[i][col] >= input[row][col]) {
            break;
        }
    }
    for (size_t i = row+1; i < height; i++) {
        res[1] += 1;
        if (input[i][col] >= input[row][col]) {
            break;
        }
    }
    for (size_t j = col+1; j < width; j++) {
        res[2] += 1;
        if (input[row][j] >= input[row][col]) {
            break;
        }
    }
    for (int j = col-1; j >= 0; j--) {
        res[3] += 1;
        if (input[row][j] >= input[row][col]) {
            break;
        }
    }
    return res[0] * res[1] * res[2] * res[3];
}
