#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define CHUNK 100

typedef struct {
    int *array;
    int cap;
    int length;
} IntList;

IntList *IntList_new(void);
void IntList_destroy(IntList *il);
void IntList_append(IntList *il, int n);
int compare (void const *a, void const *b);

int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("Usage: %s FILENAME\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *input = fopen(argv[1], "r");
    if (input == NULL) {
        printf("Error: could not open file\n");
        return EXIT_FAILURE;
    }

    int calories;
    int current_total = 0;
    IntList *totals = IntList_new();
    char line[BUFSIZ];

    while (fgets(line, BUFSIZ, input)) {
        int ret = sscanf(line, "%d", &calories);
        if (ret > 0)
            current_total += calories;
        else {
            IntList_append(totals, current_total);
            current_total = 0;
        }
    }

    if (ferror(input)) {
        perror("Error when reading file");
        IntList_destroy(totals);
        return EXIT_FAILURE;
    }
    if (fclose(input)) {
        perror("Error when reading file");
        IntList_destroy(totals);
        return EXIT_FAILURE;
    };

    // Because input file may not end on a newline character, there may be
    // a pending total
    if (current_total > 0)
        IntList_append(totals, current_total);

    qsort(totals->array, totals->length, sizeof(int), compare);

    // Part 1
    printf("%d\n", totals->array[0]);

    // Part 2
    printf("%d\n", totals->array[0] + totals->array[1] + totals->array[2]);

    IntList_destroy(totals);

    return EXIT_SUCCESS;
}

IntList *IntList_new(void) {
    IntList *new = malloc(sizeof(IntList));
    if (new == NULL) {
        perror("Error allocating IntList");
        exit(EXIT_FAILURE);
    }

    new->array = calloc(CHUNK, sizeof(int));
    if (new->array == NULL) {
        perror("Error allocating IntList array");
        free(new);
        exit(EXIT_FAILURE);
    }

    new->cap = CHUNK;
    new->length = 0;

    return new;
}

void IntList_destroy(IntList *il) {
    free(il->array);
    free(il);
    il = NULL;
}

void IntList_append(IntList *il, int n) {
    if (il->length == il->cap) {
        il->cap += CHUNK;
        il->array = realloc(il->array, il->cap * sizeof(int));
        if (il->array == NULL) {
            perror("Error allocating IntList array");
            free(il);
            exit(EXIT_FAILURE);
        }
    }
    il->array[il->length++] = n;
}

int compare (void const *a, void const *b)
{
   int const *pa = a;
   int const *pb = b;

   return -(*pa - *pb);
}
