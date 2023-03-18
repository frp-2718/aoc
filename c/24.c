#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "utils/input.h"

typedef enum {
    RIGHT,
    LEFT,
    DOWN,
    UP
} MapType;

typedef struct {
    MapType type;
    size_t width;
    size_t height;
    int offset;
    bool **grid;
} Map;

typedef struct {
    int row;
    int col;
} Coord;

int parse_input(char **input, Map map[static 4]);
void next_state(Map m[static 4]);
int shortest_path(Map blizzards[static 4], Coord start, Coord end);

Coord start_pos, end_pos;

int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("Usage: %s FILENAME\n", argv[0]);
        return EXIT_FAILURE;
    }

    char **input = file_to_lines(argv[1]);
    if (!input) {
        perror("Error reading file");
        return EXIT_FAILURE;
    }

    Map blizzard_map[4] = {0};
    if (parse_input(input, blizzard_map) != 0)
        goto clean;

    // part 1
    start_pos = (Coord){ -1, 0 };
    end_pos = (Coord){ blizzard_map[0].height, blizzard_map[0].width-1 };
    int len_path = shortest_path(blizzard_map, start_pos, end_pos);
    if (len_path == -1)
        goto clean;

    printf("%d\n", len_path);

    // part 2
    next_state(blizzard_map);
    start_pos = (Coord){ end_pos.row, end_pos.col };
    end_pos = (Coord){ -1, 0 };

    int new_len_path = shortest_path(blizzard_map, start_pos, end_pos) + 1;
    if (new_len_path == -1)
        goto clean;
    else
        len_path += new_len_path;

    next_state(blizzard_map);
    start_pos = (Coord){ -1, 0};
    end_pos = (Coord){ blizzard_map[0].height, blizzard_map[0].width-1 };

    new_len_path = shortest_path(blizzard_map, start_pos, end_pos) + 1;
    if (new_len_path == -1)
        goto clean;
    else
        len_path += new_len_path;

    printf("%d\n", len_path);

clean:
    for (size_t n = 0; n < 4; n++) {
        for (size_t i = 0; i < blizzard_map[0].height; i++) {
            free(blizzard_map[n].grid[i]);
        }
        free(blizzard_map[n].grid);
    }

    free_lines(input);

    return EXIT_SUCCESS;
}

// Ad hoc unsafe modulo
int mod(int x,int n) {
    return (x % n + n) % n;
}

void next_state(Map m[static 4]) {
    for (size_t i = 0; i < 4; i++) {
        if (i % 2 == 0)
            m[i].offset -= 1;
        else
            m[i].offset += 1;
    }
}

bool ref_map(Map m, Coord c) {
    int off_row, off_col;
    switch (m.type) {
        case RIGHT:
        case LEFT:
            off_row = c.row;
            off_col = mod(c.col + m.offset, m.width);
            break;
        case UP:
        case DOWN:
            off_row = mod(c.row + m.offset, m.height);
            off_col = c.col;
    }
    return m.grid[off_row][off_col];
}

int init_maps(char **input, Map map[static 4]) {
    size_t width = strlen(input[0]) - 1; // ignore newline character
    char **end = input;
    while (*++end);
    size_t height = end - input;

    for (size_t n = 0; n < 4; n++) {
        map[n].grid = calloc(height, sizeof(bool *));
        if (map[n].grid == NULL)
            goto error;

        for (size_t i = 0; i < height; i++) {
            map[n].grid[i] = calloc(width, sizeof(bool));
            if (map[n].grid[i] == NULL)
                goto error;
        }
        map[n].width = width - 2;
        map[n].height = height - 2;
        map[n].type = (MapType)n;
    }

    return 0;

error:
    for (size_t n = 0; n < 4; n++) {
        for (size_t i = 0; i < height; i++) {
            free(map[n].grid[i]);
        }
        free(map[n].grid);
    }
    return 1;
}

int parse_input(char **input, Map map[static 4]) {
    if (init_maps(input, map) != 0)
        return 1;

    for (size_t i = 1; i <= map[0].height; i++) {
        for (size_t j = 1; j <= map[0].width; j++) {
            switch (input[i][j]) {
            case '>':
                map[RIGHT].grid[i-1][j-1] = true;
                break;
            case '<':
                map[LEFT].grid[i-1][j-1] = true;
                break;
            case 'v':
                map[DOWN].grid[i-1][j-1] = true;
                break;
            case '^':
                map[UP].grid[i-1][j-1] = true;
                break;
            default:
                break;
            }
        }
    }

    return 0;
}

bool is_clear(Coord c, Map m[static 4]) {
    bool res = c.row >= 0 && c.row < (int)m[0].height
        && c.col >= 0 && c.col < (int)m[0].width
        && (!ref_map(m[0], c) && !ref_map(m[1], c)
                && !ref_map(m[2], c) && !ref_map(m[3], c));

    bool start = c.row == start_pos.row && c.col == start_pos.col;
    bool end = c.row == end_pos.row && c.col == end_pos.col;

    return res || start || end;
}

void next_pos(Map blizzards[static 4], bool **positions, Coord base) {
    Coord left = (Coord){ base.row, base.col - 1 };
    Coord right = (Coord){ base.row, base.col + 1 };
    Coord up = (Coord){ base.row + 1, base.col };
    Coord down = (Coord){ base.row - 1, base.col };
    if (is_clear(up, blizzards))
        positions[up.row+1][up.col] = true;
    if (is_clear(down, blizzards))
        positions[down.row+1][down.col] = true;
    if (is_clear(right, blizzards))
        positions[right.row+1][right.col] = true;
    if (is_clear(left, blizzards))
        positions[left.row+1][left.col] = true;
}

int shortest_path(Map blizzards[static 4], Coord start, Coord end) {
    size_t pos_height = blizzards[0].height + 2;
    size_t pos_width = blizzards[1].width;
    int ret = 0;

    bool **positions = calloc(pos_height, sizeof(bool *));
    if (positions == NULL) {
        ret = -1;
        goto error_2;
    }

    for (size_t i = 0; i < pos_height; i++) {
        positions[i] = calloc(pos_width, sizeof(bool));
        if (positions[i] == NULL) {
            ret = -1;
            goto error_1;
        }
    }

    positions[start.row+1][start.col] = true;

    while (!positions[end.row+1][end.col]) {
        bool **temp = calloc(pos_height, sizeof(bool *));
        if (temp == NULL) {
            ret = -1;
            goto error_1;
        }
        for (size_t i = 0; i < pos_height; i++) {
            temp[i] = calloc(pos_width, sizeof(bool));
            if (temp[i] == NULL) {
                free(temp);
                ret = -1;
                goto error_1;
            }
        }
        next_state(blizzards);
        for (size_t i = 0; i < pos_height; i++) {
            for (size_t j = 0; j < pos_width; j++) {
                if (positions[i][j]) {
                    Coord current = { i-1, j };
                    next_pos(blizzards, temp, current);
                    if (is_clear(current, blizzards))
                        temp[i][j] = true;
                }
            }
        }
        positions = temp;
        ret++;
    }

error_1:
    for (size_t i = 0; i < pos_height; i++)
        free(positions[i]);

error_2:
    free(positions);
    return ret;
}
