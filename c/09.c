#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "utils/input.h"

#define SET_CHUNK 20

typedef struct {
    int x;
    int y;
} Coord;

typedef struct {
    Coord **set;
    int capacity;
    int length;
} Coordset;

void env_init(Coordset **visited, Coord ***rope, Coord **tail_pos, size_t size);
void simulate(Coordset **visited, Coord ***rope, Coord **tail_pos, char** input, size_t size);

Coord *make_coord(int x, int y);
bool coordeq(Coord *c1, Coord *c2);
void print_coord(Coord *c);

Coordset *make_coordset(void);
Coord *add_coord(Coordset *set, Coord *c);
void destroy_coordset(Coordset *cs);
void print_coordset(Coordset *cs);

Coord **make_rope(int size);
void destroy_rope(Coord **r);
void print_rope(Coord **r);
void move_tail(Coord **r, size_t size);

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

    Coordset *visited = NULL;
    Coord **rope = NULL;
    Coord *tail_pos = NULL;

    // part 1
    env_init(&visited, &rope, &tail_pos, 2);
    simulate(&visited, &rope, &tail_pos, input, 2);

    printf("%d\n", visited->length);

    destroy_rope(rope);
    destroy_coordset(visited);

    // part 2
    env_init(&visited, &rope, &tail_pos, 10);
    simulate(&visited, &rope, &tail_pos, input, 10);

    printf("%d\n", visited->length);

    destroy_rope(rope);
    destroy_coordset(visited);

    free_lines(input);

    return EXIT_SUCCESS;
}

bool coordeq(Coord *c1, Coord *c2) {
    return c1->x == c2->x && c1->y == c2->y;
}

Coord *make_coord(int x, int y) {
    Coord *new = malloc(sizeof(Coord));
    if (new == NULL) {
        perror("Failed to allocate new Coord");
        return NULL;
    }
    new->x = x;
    new->y = y;
    return new;
}

Coordset *make_coordset(void) {
    Coordset *new = malloc(sizeof(Coordset));
    if (new == NULL) {
        perror("Failed to allocate new Coordset");
        return NULL;
    }
    new->capacity = SET_CHUNK;
    new->length = 0;
    new->set = malloc(sizeof(Coord *) * SET_CHUNK);
    if (new->set == NULL) {
        free(new);
        perror("Failed to allocate Coord **");
        return NULL;
    }
    return new;
}

void print_coord(Coord *c) {
    printf("(%d, %d)", c->x, c->y);
}

Coord *add_coord(Coordset *cs, Coord *c) {
    for (int i = 0; i < cs->length; i++) {
        if (coordeq(cs->set[i], c)) {
            return c;
        }
    }
    if (cs->length == cs->capacity) {
        cs->set = realloc(cs->set, sizeof(Coord *) * (cs->capacity + SET_CHUNK));
        if (cs->set == NULL) {
            perror("Failed to reallocate set");
            return NULL;
        }
        cs->capacity += SET_CHUNK;
    }
    cs->set[cs->length++] = c;
    return c;
}

void destroy_coordset(Coordset *cs) {
    for (int i = 0; i < cs->length; i++) {
        free(cs->set[i]);
    }
    free(cs->set);
    free(cs);
}

void print_coordset(Coordset *cs) {
    printf("{");
    for (int i = 0; i < cs->length; i++) {
        print_coord(cs->set[i]);
        if (i < cs->length-1)
            printf(", ");
    }
    printf("}\n");
}

void move_tail(Coord **r, size_t size) {
    for (int i = size - 2; i >= 0; i--) {
        if (r[i+1]->x - r[i]->x == 2 && r[i+1]->y - r[i]->y == 0)
            r[i]->x += 1;
        else if (r[i+1]->x - r[i]->x == -2 && r[i+1]->y - r[i]->y == 0)
            r[i]->x -= 1;
        else if (r[i+1]->x - r[i]->x == 0 && r[i+1]->y - r[i]->y == 2)
            r[i]->y += 1;
        else if (r[i+1]->x - r[i]->x == 0 && r[i+1]->y - r[i]->y == -2)
            r[i]->y -= 1;
        else if (r[i+1]->x - r[i]->x == -1 && r[i+1]->y - r[i]->y == 2) {
            r[i]->x -= 1;
            r[i]->y += 1;
        }
        else if (r[i+1]->x - r[i]->x == 1 && r[i+1]->y - r[i]->y == -2) {
            r[i]->x += 1;
            r[i]->y -= 1;
        }
        else if (r[i+1]->x - r[i]->x == -2 && r[i+1]->y - r[i]->y == 1) {
            r[i]->x -= 1;
            r[i]->y += 1;
        }
        else if (r[i+1]->x - r[i]->x == 2 && r[i+1]->y - r[i]->y == -1) {
            r[i]->x += 1;
            r[i]->y -= 1;
        }
        else if (r[i+1]->x - r[i]->x == 1 && r[i+1]->y - r[i]->y == 2) {
            r[i]->x += 1;
            r[i]->y += 1;
        }
        else if (r[i+1]->x - r[i]->x == -1 && r[i+1]->y - r[i]->y == -2) {
            r[i]->x -= 1;
            r[i]->y -= 1;
        }
        else if (r[i+1]->x - r[i]->x == -2 && r[i+1]->y - r[i]->y == -1) {
            r[i]->x -= 1;
            r[i]->y -= 1;
        }
        else if (r[i+1]->x - r[i]->x == 2 && r[i+1]->y - r[i]->y == 1) {
            r[i]->x += 1;
            r[i]->y += 1;
        }
        else if (r[i+1]->x - r[i]->x == 2 && r[i+1]->y - r[i]->y == 2) {
            r[i]->x += 1;
            r[i]->y += 1;
        }
        else if (r[i+1]->x - r[i]->x == -2 && r[i+1]->y - r[i]->y == 2) {
            r[i]->x -= 1;
            r[i]->y += 1;
        }
        else if (r[i+1]->x - r[i]->x == -2 && r[i+1]->y - r[i]->y == -2) {
            r[i]->x -= 1;
            r[i]->y -= 1;
        }
        else if (r[i+1]->x - r[i]->x == 2 && r[i+1]->y - r[i]->y == -2) {
            r[i]->x += 1;
            r[i]->y -= 1;
        }
    }
}

Coord **make_rope(int size) {
    Coord **rope = malloc(sizeof(Coord *) * size + 1);
    for (int i = 0; i < size; i++) {
        rope[i] = make_coord(0, 0);
        if (rope[i] == NULL)
            return NULL;
    }
    rope[size] = NULL;
    return rope;
}

void destroy_rope(Coord **r) {
    //for (int i = 0; i < ROPE_SIZE; i++) {
    //    free(r[i]);
    //}
    free(r);
}

void print_rope(Coord **r) {
    for (int i = 0; r[i]; i++) {
        print_coord(r[i]);
        printf(" ");
    }
    printf("\n");
}

void env_init(Coordset **visited, Coord ***rope, Coord **tail_pos, size_t size) {
    *visited = make_coordset();
    if (*visited == NULL) {
        exit(EXIT_FAILURE);
    }
    *rope = make_rope(size);
    if (rope == NULL) goto error_set;

    *tail_pos = make_coord((*rope)[0]->x, (*rope)[0]->y);
    if (*tail_pos == NULL) goto error_rope;

    *tail_pos = add_coord(*visited, *tail_pos);
    if (*tail_pos == NULL) goto error_rope;

    return;

error_rope:
    destroy_rope(*rope);
error_set:
    destroy_coordset(*visited);
    exit(EXIT_FAILURE);
}

void simulate(Coordset **visited, Coord ***rope, Coord **tail_pos, char** input, size_t size) {
    char dir = '\0';
    int dist = 0;
    while (*input) {
        if (sscanf(*input, "%c %d", &dir, &dist) != 2) {
            fprintf(stderr, "Error while reading input\n");
            destroy_coordset(*visited);
            exit(EXIT_FAILURE);
        }
        for (int i = 0; i < dist; i++) {
            switch (dir) {
            case 'R':
                (*rope)[size-1]->x++;
                break;
            case 'L':
                (*rope)[size-1]->x--;
                break;
            case 'U':
                (*rope)[size-1]->y++;
                break;
            case 'D':
                (*rope)[size-1]->y--;
                break;
            default:
                fprintf(stderr, "Error while reading input\n");
                destroy_coordset(*visited);
                exit(EXIT_FAILURE);
            }
            move_tail(*rope, size);
            *tail_pos = make_coord((*rope)[0]->x, (*rope)[0]->y);
            if (*tail_pos == NULL) {
                destroy_rope(*rope);
                destroy_coordset(*visited);
                exit(EXIT_FAILURE);
            }
            *tail_pos = add_coord(*visited, *tail_pos);
            if (tail_pos == NULL) {
                destroy_rope(*rope);
                destroy_coordset(*visited);
                exit(EXIT_FAILURE);
            }
        }
        input++;
    }
}
