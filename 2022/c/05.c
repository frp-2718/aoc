#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "utils/data.h"
#include "utils/input.h"

typedef struct {
    int num;
    int from;
    int to;
} Command;

typedef struct {
    Stack **cargo;
    size_t nstacks;
    Stack *instructions;
} Process;

Process *parse_input(char **input);
void free_process(Process *p);
void execute(Process *p, bool multi);

int main(int argc, char **argv) {
    if (argc != 2) {
        printf("Usage: %s FILENAME\n", argv[0]);
        return EXIT_FAILURE;
    }

    char **file_contents = file_to_lines(argv[1]);
    if (file_contents == NULL) {
        return EXIT_FAILURE;
    }

    Process *p1 = parse_input(file_contents);
    Process *p2 = parse_input(file_contents);

    execute(p1, false);
    execute(p2, true);

    // part 1
    for (size_t i = 0; i < p1->nstacks; i++) {
        char c;
        stack_pop(p1->cargo[i], (void *)&c);
        printf("%c", c);
    }
    putchar('\n');

    // part 2
    for (size_t i = 0; i < p2->nstacks; i++) {
        char c;
        stack_pop(p2->cargo[i], (void *)&c);
        printf("%c", c);
    }
    putchar('\n');

    free_process(p1);
    free_process(p2);
    free_lines(file_contents);

    return EXIT_SUCCESS;
}

Process *parse_input(char **input) {
    Process *p = malloc(sizeof(Process));
    if (!p) return NULL;

    size_t last_line = 0;
    while (input[last_line]) last_line++;
    last_line--;

    Stack *program = stack_create(sizeof(Command));
    if (!program) {
        free(p);
        return NULL;
    }

    int stst = last_line;
    while (strlen(input[stst]) > 1) {
        Command *c = malloc(sizeof(Command));
        if (!c) {
            stack_destroy(&program);
            free(p);
            return NULL;
        }
        sscanf(input[stst], "move %d from %d to %d\n", &c->num, &c->from, &c->to);
        stack_push(program, (void *)c);
        stst--;
    }

    size_t nstacks = strlen(input[0]) / 4;
    Stack **cargo = malloc(sizeof(Stack *) * nstacks);
    for (size_t i = 0; i < nstacks; i++) {
        cargo[i] = stack_create(sizeof(char));
        if (!cargo[i]) {
            for (size_t j = 0; j < i; j++) stack_destroy(&cargo[j]);
            free(cargo);
            stack_destroy(&program);
            free(p);
            return NULL;
        }
    }

    stst -= 2;
    while (stst >= 0) {
        for (size_t i = 1; i < (nstacks * 4) - 2; i += 4) {
            char c = input[stst][i];
            if (!isspace(c)) {
                stack_push(cargo[i/4], (void *)&c);
            }
        }
        stst--;
    }
    p->cargo = cargo;
    p->instructions = program;
    p->nstacks = nstacks;
    return p;
}

void free_process(Process *p) {
    if (!p) return;
    if (p->nstacks) {
        for (size_t i = 0; i < p->nstacks; i++) {
            stack_destroy(&p->cargo[i]);
        }
    }
    free(p);
}

void execute(Process *p, bool multi) {
    size_t ne = stack_len(p->instructions);
    Stack *temp;
    if (multi) {
        temp = stack_create(sizeof(char));
        if (!temp) {
            perror("Failed to allocate memory");
            exit(1);
        }

    }
    for (size_t i = 0; i < ne; i++) {
        Command cmd;
        stack_pop(p->instructions, &cmd);
        char *c = malloc(sizeof(char));
        for (int n = 0; n < cmd.num; n++) {
            if (!c) {
                perror("Failed to allocate memory");
                exit(1);
            }
            stack_pop(p->cargo[cmd.from-1], c);
            if (multi) {
                stack_push(temp, (void *)c);
            } else {
                stack_push(p->cargo[cmd.to-1], (void *)c);
            }
        }
        if (multi) {
            for (int j = 0; j < cmd.num; j++) {
                stack_pop(temp, c);
                stack_push(p->cargo[cmd.to-1], (void *)c);
            }
        }
    }
}
