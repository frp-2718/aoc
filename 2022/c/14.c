#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "utils/input.h"

#define NWALLS 50
#define MAX(a, b) ((a) > (b) ? (a) : (b))

typedef struct {
    size_t x;
    size_t y;
} Coord;

typedef struct {
    Coord from;
    Coord to;
} Interval;

typedef struct {
    size_t min_x, max_x;
    size_t min_y, max_y;
    size_t nw;
} Dimension;

Coord to_coord(char *str);
Interval *parse_input(char **input, Dimension *dim);
int simulation(size_t height, size_t width, char map[height][width], size_t offset);
void init_map(size_t height, size_t width, char map[height][width]);
void build_walls(size_t height, size_t width, char map[height][width], size_t nw, Interval *walls, int offset);

int main(int argc, char* argv[]) {
    if (argc != 2) {
        printf("Usage: %s FILENAME\n", argv[0]);
        return EXIT_FAILURE;
    }

    char **input = file_to_lines(argv[1]);
    if (!input) {
        perror("Error reading file");
        return EXIT_FAILURE;
    }

    Dimension dim = { .min_x = INT_MAX, .min_y = 0, .max_x = 0, .max_y = 0, .nw = 0};
    Interval *walls = parse_input(input, &dim);

    free_lines(input);

    // part 1
    size_t height = dim.max_y - dim.min_y + 1;
    size_t width = dim.max_x - dim.min_x + 1;
    
    char map[height][width];

    init_map(height, width, map);
    build_walls(height, width, map, dim.nw, walls, dim.min_x);

    int n = 0;
    while (simulation(height, width, map, dim.min_x)) {
        n++;
    }

    printf("%d\n", n);

    // part 2
    height = height + 2;
    width = MAX(500 - dim.min_x, dim.max_x - 500) * 4;
    width = MAX(width, height * 2);
    if (width % 2 == 0) width += 1;

    char map2[height][width];
    int offset = 500 - (width/2);

    init_map(height, width, map2);
    build_walls(height, width, map2, dim.nw, walls, offset);
    for (size_t i = 0; i < width; i++) {
        map2[height-1][i] = '#';
    }

    n = 0;
    while (simulation(height, width, map2, offset)) {
        n++;
    }

    printf("%d\n", n);

    free(walls);

    return EXIT_SUCCESS;
}

void build_walls(size_t height, size_t width, char map[height][width],
        size_t nw, Interval *walls, int offset) {
    for (size_t i = 0; i < nw; i++) {
        if (walls[i].from.y < walls[i].to.y) {
            size_t col = walls[i].from.x - offset;
            for (size_t row = walls[i].from.y; row <= walls[i].to.y; row++) {
                map[row][col] = '#';
            }
        } else {
            size_t row = walls[i].from.y; 
            for (size_t col = walls[i].from.x - offset; col <= walls[i].to.x - offset; col++) {
                map[row][col] = '#';
            }
        }
    }
}

void init_map(size_t height, size_t width, char map[height][width]) {
    for (size_t i = 0; i < height; i++) {
        for (size_t j = 0; j < width; j++) {
            map[i][j] = '.';
        }
    }
}

int simulation(size_t height, size_t width, char map[height][width], size_t offset) {
    size_t col = 500 - offset;
    size_t row = 0;
    if (map[row][col] == 'o') {
        return 0;
    }
    while (1) {
        if (map[row+1][col] == '.') {
            row++;
        } else if (col-1 > width || row + 1 >= height) {
            return 0;
        } else if (map[row+1][col-1] == '.') {
            row++;
            col--;
        } else if (map[row+1][col+1] == '.') {
            row++;
            col++;
        } else {
            map[row][col] = 'o';
            return 1;
        }
    }
}

Coord to_coord(char *token) {
    Coord res = {0};
    sscanf(token, "%ld,%ld", &res.x, &res.y);
    return res;
}

Interval *parse_input(char **input, Dimension *dim) {
    Interval *walls = calloc(NWALLS, sizeof(Interval));
    if (walls == NULL) {
        perror("Error allocating memory");
        exit(EXIT_FAILURE);
    }

    // Assume coord (0,0) does not exist.
    for (char **current = input; *current; current++) {
        char *token = strtok(*current, ">- \n");
        while (token) {
            Coord c = to_coord(token);
            if (c.x < dim->min_x) dim->min_x = c.x;
            if (c.x > dim->max_x) dim->max_x = c.x;
            if (c.y > dim->max_y) dim->max_y = c.y;

            if (walls[dim->nw].from.x == 0 && walls[dim->nw].from.y == 0) {
                walls[dim->nw].from = c;
            } else {
                if (c.x < walls[dim->nw].from.x || c.y < walls[dim->nw].from.y) {
                    walls[dim->nw].to = walls[dim->nw].from;
                    walls[dim->nw].from = c;
                } else {
                    walls[dim->nw].to = c;
                }
                dim->nw++;
                if (dim->nw >= NWALLS) {
                    walls = realloc(walls, dim->nw * sizeof(Interval) * 2);
                    if (walls == NULL) {
                        perror("Error allocating memory");
                        exit(EXIT_FAILURE);
                    }
                }
                walls[dim->nw].from = c;
            }
            token = strtok(NULL, ">- \n");
        }
        walls[dim->nw].from.x = 0;
        walls[dim->nw].from.y = 0;
    }
    return walls;
}
