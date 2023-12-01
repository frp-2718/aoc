#include <errno.h>
#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "utils/data.h"
#include "utils/input.h"

typedef struct {
    int row;
    int col;
} Coord;

void revert(char **input, int height, int width);
void prepare_grid(char **input, int height, int width, Coord *start, Coord *end);
size_t dijkstra(char **grid, int cols, int rows, Coord start, Coord end, bool test);

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
    char **last = input;
    while (*last++);
    size_t height = last - input - 1;
    Coord start, end;

    // part 1
    prepare_grid(input, height, width, &start, &end);

    size_t min_dist = dijkstra(input, width, height, start, end, false);
    printf("%zu\n", min_dist);

    // part2
    input[start.row][start.col] = 'S';
    input[end.row][end.col] = 'E';
    revert(input, height, width);
    prepare_grid(input, height, width, &start, &end);

    min_dist = dijkstra(input, width, height, start, end, true);
    printf("%zu\n", min_dist);

    free_lines(input);

    return EXIT_SUCCESS;
}

void prepare_grid(char **input, int height, int width, Coord *start, Coord *end) {
    for (int i = 0; i < height; i++) {
        for (int j = 0; j < width; j++) {
            if (input[i][j] == 'S')
                *start = (Coord){ i, j };
            else if (input[i][j] == 'E')
                *end = (Coord){ i, j };
        }
    }

    input[start->row][start->col] = 'a';
    input[end->row][end->col] = 'z';
}

void revert(char **input, int height, int width) {
    for (int row = 0; row < height; row++) {
        for (int col = 0; col < width; col++) {
            if (input[row][col] == 'E')
                input[row][col] = 'S';
            else if (input[row][col] == 'S')
                input[row][col] = 'E';
            else
                input[row][col] = 'z' - input[row][col] + 'a';
        }
    }
}

bool is_valid(Coord c, int height, int width, bool visited[][width]) {
    return (c.row >= 0 && c.row < height) &&
        (c.col >= 0 && c.col < width) &&
        !visited[c.row][c.col];
}

size_t dijkstra(char **grid, int cols, int rows, Coord start, Coord end, bool test) {
    bool visited[rows][cols];
    bool to_be_visited[rows][cols];
    size_t dist[rows][cols];
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            visited[i][j] = false;
            to_be_visited[i][j] = true;
            dist[i][j] = INT_MAX;
        }
    }

    dist[start.row][start.col] = 0;

    List *to_visit = list_new(Coord);
    list_insertr(to_visit, &start);
    Coord pos = start;
    size_t min_dist = INT_MAX;

    while (pos.row != end.row || pos.col != end.col) {
        list_popl(to_visit, &pos);
        visited[pos.row][pos.col] = true;

        Coord dir[4];
        dir[0] = (Coord){ pos.row - 1, pos.col };
        dir[1] = (Coord){ pos.row + 1, pos.col };
        dir[2] = (Coord){ pos.row, pos.col - 1 };
        dir[3] = (Coord){ pos.row, pos.col + 1 };

        for (size_t i = 0; i < 4; i++) {
            if (is_valid(dir[i], rows, cols, visited) &&
                    grid[pos.row][pos.col] >= grid[dir[i].row][dir[i].col] - 1) {
                if (dist[pos.row][pos.col] + 1 < dist[dir[i].row][dir[i].col]) {
                    dist[dir[i].row][dir[i].col] = dist[pos.row][pos.col] + 1;
                }
                if (to_be_visited[dir[i].row][dir[i].col]) {
                    list_insertr(to_visit, &dir[i]);
                    to_be_visited[dir[i].row][dir[i].col] = false;
                }
            }
        }
        if (test && grid[pos.row][pos.col] == 'z') {
            min_dist = dist[pos.row][pos.col];
            break;
        }
    }

    list_destroy(&to_visit);

    return (min_dist < INT_MAX) ? min_dist : dist[end.row][end.col];
}
