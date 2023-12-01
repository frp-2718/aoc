#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "utils/input.h"

#define CHUNK 100

typedef enum {
    PLUS,
    MULT,
} Op;

typedef struct {
    long *items;
    long nitems;
    Op operation;
    long operand;
    long divisible;
    long iftrue;
    long iffalse;
    long inspected;
} Monkey;

int cmp(const void *a,const void *b);
size_t parse_monkeys(char *input, Monkey monkeys[]);
void execute(int nrounds, Monkey monkeys[], size_t nmonkeys, long (*wf)(long, long), long prod);
long compute_worry_2(long w, long prod);
long compute_worry_1(long w, long prod);
long divisor_product(Monkey ms[], size_t nmonkeys);

int main(int argc, char **argv) {
    if (argc != 2) {
        printf("Usage: %s FILENAME\n", argv[0]);
        return EXIT_FAILURE;
    }

    char *input = file_to_string(argv[1]);
    if (!input) {
        perror("Error reading file");
        return EXIT_FAILURE;
    }

    char *input_cpy = malloc(strlen(input) + 1);
    if (!input_cpy) return EXIT_FAILURE;
    strcpy(input_cpy, input);

    // part 1
    Monkey monkeys[10];
    size_t nmonkeys = parse_monkeys(input, monkeys);

    execute(20, monkeys, nmonkeys, compute_worry_1, 0);

    long inspected[10] = {0};
    for (size_t i = 0; i < nmonkeys; i++) {
        inspected[i] = monkeys[i].inspected;
    }
    qsort(inspected, 10, sizeof(long), cmp);

    printf("%lu\n", inspected[0] * inspected[1]);

    // part 2
    nmonkeys = parse_monkeys(input_cpy, monkeys);
    long divprod = divisor_product(monkeys, nmonkeys);
    execute(10000, monkeys, nmonkeys, compute_worry_2, divprod);


    for (size_t i = 0; i < nmonkeys; i++) {
        inspected[i] = monkeys[i].inspected;
    }
    qsort(inspected, 10, sizeof(long), cmp);

    printf("%lu\n", inspected[0] * inspected[1]);

    free(input);

    return EXIT_SUCCESS;
}

int cmp(const void *a,const void *b) {
    long x = *(long *) a;
    long y = *(long *) b;
    return y - x;
}

size_t parse_monkeys(char *input, Monkey monkeys[]) {
    const char *sep = " ,:\n";
    size_t nmonkeys = 0;
    char *token = strtok(input, sep);

    while(token) {
        if (strstr(token, "Monkey")) {
            long *items = malloc(CHUNK * sizeof(long));
            monkeys[nmonkeys++] = (Monkey) {items, 0, PLUS, 0, 0, 0, 0, 0};
            token = strtok(NULL, sep);
        } else if (strcmp(token, "items") == 0) {
            long n = 0;
            token = strtok(NULL, sep);
            while ((n = strtoll(token, NULL, 10)) != 0) {
                Monkey *current = &monkeys[nmonkeys-1];
                current->items[current->nitems++] = n;
                token = strtok(NULL, sep);
            }
        } else if (strstr(token, "=")) {
            token = strtok(NULL, sep);
            token = strtok(NULL, sep);
            Monkey *current = &monkeys[nmonkeys-1];
            if (strstr(token, "+")) {
                current->operation = PLUS;
            } else if (strstr(token, "*")) {
                current->operation = MULT;
            }
            token = strtok(NULL, sep);
            long n = strtoll(token, NULL, 10);
            if (n != 0)
                current->operand = n;
            else
                current->operand = -1;
        } else if (strstr(token, "by")) {
            token = strtok(NULL, sep);
            Monkey *current = &monkeys[nmonkeys-1];
            current->divisible = strtol(token, NULL, 10);
        } else if (strstr(token, "true")) {
            for (int i = 0; i < 4; i++) token = strtok(NULL, sep);
            Monkey *current = &monkeys[nmonkeys-1];
            current->iftrue = strtoll(token, NULL, 10);
        } else if (strstr(token, "false")) {
            Monkey *current = &monkeys[nmonkeys-1];
            for (int i = 0; i < 4; i++) token = strtok(NULL, sep);
            current->iffalse = strtoll(token, NULL, 10);
        }
        token = strtok(NULL, sep);
    }
    return nmonkeys;
}

void execute(int nrounds, Monkey monkeys[], size_t nmonkeys, long (*wf)(long, long), long prod) {
    for (int round = 0; round < nrounds; round++) {
        for (size_t i = 0; i < nmonkeys; i++) {
            long nb_items = monkeys[i].nitems;;
            for (long item = 0; item < nb_items; item++) {
                monkeys[i].inspected++;
                long worry = monkeys[i].items[item];
                long operand = (monkeys[i].operand == -1) ? worry : monkeys[i].operand;
                if (monkeys[i].operation == PLUS)
                    worry += operand;
                else
                    worry *= operand;
                worry = wf(worry, prod);
                long to_throw;
                if (worry % monkeys[i].divisible == 0) {
                    to_throw = monkeys[i].iftrue;
                    Monkey *dest = &monkeys[to_throw];
                    dest->items[dest->nitems++] = worry;
                    monkeys[i].nitems--;
                } else {
                    to_throw = monkeys[i].iffalse;
                    Monkey *dest = &monkeys[to_throw];
                    dest->items[dest->nitems++] = worry;
                    monkeys[i].nitems--;
                }
            }
        }
    }
}

long compute_worry_2(long w, long prod) {
    return w % prod;
}

long compute_worry_1(long w, long prod) {
    return w = (int)((double) w / 3.0) + prod;
}

long divisor_product(Monkey ms[], size_t nmonkeys) {
    long total = 1;
    for (size_t i = 0; i < nmonkeys; i++) {
        total *= ms[i].divisible;
    }
    return total;
}
