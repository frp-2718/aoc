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
} Position;

typedef struct {
    Position pos[3];
} Direction;

Direction order[] = {
    {(Position){-1, -1}, (Position){-1, 0}, (Position){-1, 1}}, // north
    {(Position){1, -1}, (Position){1, 0}, (Position){1, 1}},    // south
    {(Position){-1, -1}, (Position){0, -1}, (Position){1, -1}}, // west
    {(Position){-1, 1}, (Position){0, 1}, (Position){1, 1}}     // east
};

char* pos_to_str(Position pos);
bool check_neighbours(Hashtbl *grid, Position p, Direction dir);
int str_to_pos(const char *str, Position *pos);
Hashtbl *make_proposition(Hashtbl *pos, int start);
void destroy_pos(void *pos);
void move_elves(Hashtbl *current, Hashtbl *prop);
size_t min_rectangle_area(Hashtbl *pos);

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

    Hashtbl *pos_p1 = hashtbl_create();
    Hashtbl *pos_p2 = hashtbl_create();
    if (!pos_p1 || !pos_p2) return EXIT_FAILURE;

    if (hashtbl_init(pos_p1, hashtbl_destroy_default_value) == -1) {
        perror("Hash table for part 1 initialize error");
        return EXIT_FAILURE;
    }
    
    if (hashtbl_init(pos_p2, hashtbl_destroy_default_value) == -1) {
        perror("Hash table for part 2 initialize error");
        return EXIT_FAILURE;
    }

    for (int row = 0; input[row]; row++) {
        for (int col = 0; input[row][col]; col++) {
            if (input[row][col] == '#') {
                char *key = pos_to_str((Position){row, col});
                hashtbl_insert(pos_p1, key, hashtbl_default_value());
                hashtbl_insert(pos_p2, key, hashtbl_default_value());
            }
        }
    }

    // Part 1
    size_t rounds = 10;
    Hashtbl *propositions;
    for (size_t i = 0; i < rounds; i++) {
        propositions = make_proposition(pos_p1, i % 4);
        move_elves(pos_p1, propositions);
        hashtbl_destroy(propositions);
    }

    printf("%zu\n", min_rectangle_area(pos_p1) - hashtbl_size(pos_p1));

    for (size_t i = 0; ; i++) {
        propositions = make_proposition(pos_p2, i % 4);
        if (!propositions) {
            printf("%zu\n", i+1);
            break;
        }
        move_elves(pos_p2, propositions);
        hashtbl_destroy(propositions);
    }

    hashtbl_destroy(pos_p1);
    hashtbl_destroy(pos_p2);
    free_lines(input);

    return EXIT_SUCCESS;
}

char* pos_to_str(Position pos) {
    char *buffer = malloc(24);
    if (!buffer) return NULL;
    sprintf(buffer, "%d,%d", pos.row, pos.col);
    return buffer;
}

int str_to_pos(const char *str, Position *pos) {
    if (sscanf(str, "%d,%d", &pos->row, &pos->col) == 2) {
        return 1;
    }
    return 0;
}

bool check_neighbours(Hashtbl *grid, Position p, Direction dir) {
    for (int i = 0; i < 3; i++) {
        Position new_p = {p.row + dir.pos[i].row, p.col + dir.pos[i].col};
        char *key = pos_to_str(new_p);
        void *found = hashtbl_lookup(grid, key);
        free(key);

        if (found && *(char *)found == '0') {
            return true;
        }
    }
    return false;
}

bool check_all_neighbours(Hashtbl *grid, Position p) {
    for (size_t i = 0; i < 4; i++) {
        if (check_neighbours(grid, p, order[i]))
            return true;
    }
    return false;
}

Hashtbl *make_proposition(Hashtbl *pos, int start) {
    bool move = false;
    Hashtbl *prop = hashtbl_create();
    hashtbl_init(prop, destroy_pos);

    HashtblIterator *it = new_hashtblIterator(pos);
    Entry *e;
    while ((e = hashtblIterator_next(it)) != NULL) {
        bool ongoing = true;
        Position *p = malloc(sizeof(Position));
        str_to_pos(e->key, p);
        if (!check_all_neighbours(pos, *p)) {
            free(p);
            continue;
        }
        for (size_t i = 0; i < 4; i++) {
            size_t idx = (start + i) % 4;
            if (!check_neighbours(pos, *p, order[idx])) {
                ongoing = false;
                Position new_pos = (Position){p->row + order[idx].pos[1].row, p->col + order[idx].pos[1].col};
                char *str_pos = pos_to_str(new_pos);
                if (hashtbl_lookup(prop, str_pos)) {
                    p = NULL;
                }
                hashtbl_insert(prop, str_pos, (void *)p);
                move = true;
                free(str_pos);
                break;
            }
        }
        if (!ongoing) {
            ongoing = true;
            continue;
        }
    }

    hashtblIterator_destroy(&it);
    return move ? prop : NULL;
}

void destroy_pos(void *pos) {
    if (pos) free(pos);
}

void move_elves(Hashtbl *current, Hashtbl *prop) {
    HashtblIterator *it = new_hashtblIterator(prop);
    Entry *e;
    while ((e = hashtblIterator_next(it)) != NULL) {
        if (e->value) {
            Position *p = (Position *)e->value;
            char *str_pos = pos_to_str(*p);
            hashtbl_remove(current, str_pos);
            hashtbl_insert(current, e->key, hashtbl_default_value());
            free(str_pos);
        }
    }
    free(it);
}

size_t min_rectangle_area(Hashtbl *pos) {
    HashtblIterator *it = new_hashtblIterator(pos);
    Entry *e;
    int min_row = INT_MAX;
    int max_row = 0;
    int min_col = INT_MAX;
    int max_col = 0;
    while ((e = hashtblIterator_next(it)) != NULL) {
        Position *p = malloc(sizeof(Position));
        str_to_pos(e->key, p);
        if (p->row < min_row) {
            min_row = p->row;
        }
        if (p->row > max_row) {
            max_row = p->row;
        }
        if (p->col < min_col) {
            min_col = p->col;
        }
        if (p->col > max_col) {
            max_col = p->col;
        }
        free(p);
    }
    free(it);
    return (max_row - min_row + 1) * (max_col - min_col + 1);
}
