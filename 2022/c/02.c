#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

typedef enum { ROCK, PAPER, SCISSORS } choice;
typedef enum { LOSE, DRAW, WIN } result;

int score(choice ch1, choice ch2);
int score_strategy(choice ch1, result res);

int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("Usage: %s FILENAME\n", argv[0]);
        return EXIT_FAILURE;
    }

    errno = 0;

    FILE *in = fopen(argv[1], "r");
    if (in == NULL) {
        perror("Failed to open file");
        return EXIT_FAILURE;
    }

    int c;
    choice ch1, ch2;
    result res;
    int total1 = 0, total2 = 0;

    while ((c = fgetc(in)) != EOF) {
        switch(c) {
            case 'A':
                ch1 = ROCK;
                break;
            case 'B':
                ch1 = PAPER;
                break;
            case 'C':
                ch1 = SCISSORS;
                break;
            case 'X':
                ch2 = ROCK;
                res = LOSE;
                break;
            case 'Y':
                ch2 = PAPER;
                res = DRAW;
                break;
            case 'Z':
                ch2 = SCISSORS;
                res = WIN;
                break;
            case '\n':
                total1 += score(ch1, ch2);
                total2 += score_strategy(ch1, res);
                break;
            default:
                break;
        }
    }

    if (ferror(in)) {
        perror("Error when reading file");
        return EXIT_FAILURE;
    }
    if (fclose(in)) {
        perror("Close");
        return EXIT_FAILURE;
    }

    // part 1
    printf("%d\n", total1);

    // part 2
    printf("%d\n", total2);

    return EXIT_SUCCESS;
}

int score(choice ch1, choice ch2) {
    int s = ch2 + 1;
    if (ch2 - ch1 == 1 || ch2 - ch1 == -2) {
        s += 6;
    } else if (ch2 - ch1 == 0) {
        s += 3;
    }

    return s;
}

int score_strategy(choice ch1, result res) {
    choice ch2;
    if (res == LOSE) {
        ch2 = (ch1 + 2) % 3;
    } else if (res == DRAW) {
        ch2 = ch1;
    } else {
        ch2 = (ch1 + 1) % 3;
    }

    int s = ch2 + 1;
    if (ch2 - ch1 == 1 || ch2 - ch1 == -2) {
        s += 6;
    } else if (ch2 - ch1 == 0) {
        s += 3;
    }

    return s;
}
