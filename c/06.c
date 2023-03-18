#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "utils/input.h"

int start_of_packet(char* input, size_t size);
bool is_set(char* start, size_t len);

int main(int argc, char **argv) {
    if (argc != 2) {
        printf("Usage: %s FILENAME\n", argv[0]);
        return EXIT_FAILURE;
    }

    char **file_contents = file_to_lines(argv[1]);
    if (file_contents == NULL) {
        return EXIT_FAILURE;
    }

    // part 1
    for (size_t i = 0; file_contents[i]; i++) {
        printf("%d\n", start_of_packet(file_contents[i], 4));
    }

    // part 2
    for (size_t i = 0; file_contents[i]; i++) {
        printf("%d\n", start_of_packet(file_contents[i], 14));
    }

    free(file_contents);

    return EXIT_SUCCESS;
}

int start_of_packet(char* input, size_t size) {
    size_t i = 0;
    size_t len = strlen(input);
    while (i <= len - size) {
        if (is_set(input + i, size))
            return i + size;
        else
            i++;
    }
    return -1;
}

bool is_set(char* start, size_t len) {
    for (size_t i = 0; i < len-1; i++) {
        for (size_t j = i+1; j < len; j++) {
            if (start[i] == start[j])
                return false;
        }
    }
    return true;
}
