#ifndef TEST_H
#define TEST_H

#include <stdbool.h>
#include <stdlib.h>

#define t_assert_msg_exit(expr, abort, ...)                            \
    do {                                                               \
        if (!(expr)) {                                                 \
            fprintf(stderr, "FAIL %s: %lu: ", __func__, __LINE__+0UL); \
            fprintf(stderr, __VA_ARGS__);                              \
            if (abort) {                                               \
                fprintf(stderr, " ** abort **\n");                     \
                exit(1);                                               \
            }                                                          \
            fprintf(stderr, "\n");                                     \
        }                                                              \
    } while (0)

#define t_assert_msg(expr, ...) \
    t_assert_msg_exit(expr, false, __VA_ARGS__)

#define t_assert_exit(expr) \
    t_assert_msg_exit(expr, true, #expr " failed")

#define t_assert(expr) \
    t_assert_msg(expr, #expr " failed")

#endif // TEST_H
