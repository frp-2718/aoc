#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "utils/input.h"
#include "utils/data.h"

#define DATASIZE 256UL

List *itemize(char *s);
int listcmp(List *l1, List *l2);
int linecmp(const void *l1, const void *l2);
void print_string(void *s);
void add_packets(char **input, char *p1, char *p2);

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

    // part 1
    int total = 0;
    for (size_t i = 0; input[i]; i += 3) {
        List *l1 = itemize(input[i]);
        List *l2 = itemize(input[i+1]);
        if (listcmp(l1, l2) <= 0) {
            total += i / 3 + 1;
        }
        if (!input[i+2]) break;
    }
    printf("%d\n", total);

    // part 2
    char *packet1 = "[[2]]\n";
    char *packet2 = "[[6]]\n";
    add_packets(input, packet1, packet2);
    char **last = input;
    while (*++last);
    ptrdiff_t nlines = last - input;
    qsort(input, nlines, sizeof(char *), linecmp);

    size_t line = 0;
    size_t result = 1;
    for (size_t i = 0; input[i]; i++) {
        if (input[i][0] != '\n')
            line++;
        if (strcmp(input[i], packet1) == 0) {
            result *= line;
        } else if (strcmp(input[i], packet2) == 0) {
            result *= line;
            break;
        }
    }
    printf("%zu\n", result);

    free_lines(input);

    return EXIT_SUCCESS;
}

void add_packets(char **input, char *p1, char *p2) {
    char *sig1 = malloc(DATASIZE);
    if (sig1 == NULL) exit(EXIT_FAILURE);
    strcpy(sig1, p1);

    char *sig2 = malloc(DATASIZE);
    if (sig2 == NULL) exit(EXIT_FAILURE);
    strcpy(sig2, p2);

    input[2] = sig1;
    input[5] = sig2;
}

int linecmp(const void *line1, const void *line2) {
    char * const *str1 = line1;
    char * const *str2 = line2;

    char *s1 = malloc(DATASIZE);
    if (s1 == NULL) exit(EXIT_FAILURE);
    char *s2 = malloc(DATASIZE);
    if (s2 == NULL) exit(EXIT_FAILURE);

    memcpy(s1, *str1, DATASIZE);
    memcpy(s2, *str2, DATASIZE);
    List *l1 = itemize(s1);
    List *l2 = itemize(s2);
    int res = listcmp(l1, l2);
    free(s1);
    free(s2);
    return res;
}

void print_string(void *s) {
    printf("%s", (char *)s);
}

char *find_closing(char *s) {
    int nbrackets = 0;
    do {
        if (*s == '[') nbrackets++;
        if (*s == ']') nbrackets--;
        s++;
    } while (nbrackets > 0);
    return s-1;
}

char *find_sep(char *s) {
    while (isdigit(*s)) s++;
    return s-1;
}

List *itemize(char *s) {
    List *l = list_create(DATASIZE);
    if (l == NULL) return NULL;

    char *start = s;
    while (true) {
        start += 1;
        char *end = NULL;
        if (!*start || *start == ']') {
            return l;
        } else if (*start == '[') {
            end = find_closing(start);
        } else if (*start == ',') {
            continue;
        } else {
            end = find_sep(start);
        }
        ptrdiff_t len = end - start + 1;
        char *buffer = calloc(DATASIZE, sizeof(char));
        if (buffer == NULL) {
            list_destroy(&l);
            return NULL;
        }
        memcpy(buffer, start, len);
        list_insertr(l, (void *)buffer);
        free(buffer);
        start = end;
    }
}

bool is_list(char *it) {
    return it[0] == '[';
}

bool is_number(char *it) {
    return isdigit(*it);
}

char *convert(char *s) {
    size_t len = strlen(s);
    s[len+2] = '\0';
    s[len+1] = ']';
    for (size_t i = len; i > 0; i--) {
        s[i] = s[i-1];
    }
    s[0] = '[';
    return s;
}

int itemcmp(char *item1, char *item2) {
    if (is_list(item1) && is_list(item2)) {
        List *new1 = itemize(item1);
        List *new2 = itemize(item2);
        return listcmp(new1, new2);
    } else if (is_number(item1) && is_number(item2)) {
        long a = strtol(item1, NULL, 10);
        long b = strtol(item2, NULL, 10);
        return a - b;
    } else if (is_list(item1)) {
        List *new1 = itemize(item1);
        List *new2 = itemize(convert(item2));
        return listcmp(new1, new2);
    } else {
        List *new1 = itemize(convert(item1));
        List *new2 = itemize(item2);
        return listcmp(new1, new2);
    }
}

int listcmp(List *l1, List *l2) {
    int res = 0;
    if (list_len(l1) == 0 || list_len(l2) == 0) {
        if (list_len(l1) == 0 && list_len(l2) == 0) res = 0;
        if (list_len(l1) == 0 && list_len(l2) > 0) res =  -1;
        if (list_len(l1) > 0 && list_len(l2) == 0) res = 1;
    } else {
        while (true) {
            char *item1 = malloc(DATASIZE);
            char *item2 = malloc(DATASIZE);
            int it1 = list_popl(l1, item1);
            int it2 = list_popl(l2, item2);
            if (it1 == 0 && it2 == 0) {
                res = 0;
                break;
            } else if (it1 == 0 && it2 != 0) {
                res = -1;
                break;
            } else if (it1 != 0 && it2 == 0) {
                res = 1;
                break;
            }

            res = itemcmp(item1, item2);
            free(item1);
            free(item2);
            if (res != 0) {
                break;
            }
        }
    }

    list_destroy(&l1);
    list_destroy(&l2);

    return res;
}
