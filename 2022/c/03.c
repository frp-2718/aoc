#include <ctype.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "utils/input.h"

static char unique(char **strings, size_t nstr);
static int char_to_index(char c);
static char index_to_char(int i);
static int priority(char c);

int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("Usage: %s FILENAME\n", argv[0]);
        return EXIT_FAILURE;
    }

    char **lines = file_to_lines(argv[1]);
    if (lines == NULL) {
        return EXIT_FAILURE;
    }

    char *half[2];
    char *triple[3];
    int priorities_1 = 0;
    int priorities_2 = 0;

    for (size_t i = 0; lines[i]; i++) {

        // part 1
        size_t len = (strlen(lines[i]) - 1) / 2;
        half[0] = malloc(len * sizeof(char) + 1);
        if (half[0] == NULL) {
            perror("Error allocating memory");
            free_lines(lines);
            return EXIT_FAILURE;
        }
        memcpy(half[0], lines[i], len * sizeof(char));
        half[0][len] = '\0';
        half[1] = lines[i] + len;
        priorities_1 += priority(unique(half, 2));

        // part 2
        triple[i%3] = lines[i];
        if (i % 3 == 2) {
            priorities_2 += priority(unique(triple, 3));
        }
    }

    // part 1
    printf("%d\n", priorities_1);

    // part 2
    printf("%d\n", priorities_2);

    free_lines(lines);

    return EXIT_SUCCESS;
}

static char unique(char **strings, size_t nstr) {
    uint64_t res = 0;
    for (size_t i = 0; i < nstr; i++) {
        uint64_t current = 0;
        for (size_t j = 0; strings[i][j] && strings[i][j] != '\n'; j++) {
            int shift = char_to_index(strings[i][j]);
            uint64_t mask = 1UL << shift;
            current |= mask;
        }
        if (i == 0) res = current;
        res &= current;
    }
    int index = 0;
    while (res > 1) {
        res >>= 1;
        index++;
    }
    return index_to_char(index);
}

// A...Z   a....z
// 0...25  26...51
static int char_to_index(char c) {
    if (isupper(c)) {
        return c - 'A';
    } else {
        return c - 'a' + 26;
    }
}

// 0...25  26...51
// A...Z   a....z
static char index_to_char(int i) {
    if (i < 26) {
        return (char)(i + 'A');
    } else {
        return (char)(i - 26 + 'a');
    }
}

static int priority(char c) {
    if (isupper(c))
        return c - 'A' + 27;
    else
        return c - 'a' + 1;
}
