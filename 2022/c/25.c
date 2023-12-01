#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "utils/input.h"

#define SNAFU_MAX 30

char *decimal_to_snafu(long long n);
long long snafu_to_decimal(char *s);

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

    long long total = 0;
    for (size_t i = 0; input[i]; i++) {
        total += snafu_to_decimal(input[i]);
    }
    printf("%s\n", decimal_to_snafu(total));

    free_lines(input);

    return EXIT_SUCCESS;
}

char snafu_sym(int d) {
    switch(d) {
    case -2:
        return '=';
        break;
    case -1:
        return '-';
        break;
    case 0:
        return '0';
        break;
    case 1:
        return '1';
        break;
    case 2:
        return '2';
        break;
    default:
        return '*';
        break;
    }
}

long long decimal_sym(char c) {
    switch(c) {
    case '2':
        return 2LL;
        break;
    case '1':
        return 1LL;
        break;
    case '0':
        return 0LL;
        break;
    case '-':
        return -1LL;
        break;
    case '=':
        return -2LL;
        break;
    default:
        return 0LL;
        break;
    }
}

void reverse(char *s) {
    size_t len = strlen(s);
    for (size_t i = 0, j = len-1; i < j; i++, j--) {
        char tmp = s[i];
        s[i] = s[j];
        s[j] = tmp;
    }
}

char *decimal_to_snafu(long long n) {
    char *res = malloc(SNAFU_MAX);
    if (res == NULL)
        return NULL;

    int carry = 0;
    int len = 0;
    while (n > 0) {
        long long m = (n % 5) + carry;
        carry = m > 2 ? 1 : 0;
        m = m > 2 ? m - 5 : m;
        res[len++] = snafu_sym(m);
        n /= 5;
    }
    res[len] = '\0';
    reverse(res);
    return res;
}

long long snafu_to_decimal(char *s) {
    long long res = 0;
    size_t len = strlen(s);

    for (size_t i = 0; i < len-1; i++) {
        long long current = decimal_sym(s[i]);
        res += pow(5, len-i-2) * current;
    }

    return res;
}
