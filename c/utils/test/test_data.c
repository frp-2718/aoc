#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../data.h"
#include "test.h"

void print_int(void *elem) {
    int n = *(int *)elem;
    printf("%d", n);
}

void print_string(void *elem) {
    char **s = (char **)elem;
    printf("%s", *s);
}

void test_list_new(void) {
    List *l = list_new(int);
    t_assert_exit(l != NULL);

    void *fdata = NULL;
    void *ldata = NULL;
    int errf = list_popl(l, fdata);
    int errl = list_popr(l, ldata);

    t_assert(!(fdata || ldata || errf || errl));
    t_assert(list_len(l) == 0);
    t_assert(list_data_size(l) == sizeof(int));

    list_destroy(&l);
}

void test_list_insertr(void) {
    List *l = list_new(int);
    int n[3] = {0, 1, 2};
    for (size_t i = 0; i < 3; i++) {
        List *res = list_insertr(l, (void *)&n[i]);
        t_assert(res != NULL);
        if (!res) {
            list_destroy(&l);
            exit(1);
        }
        t_assert_msg(list_len(l) == i + 1, "expected %zu, got %zu", i + 1, list_len(l));
    }

    int addr;
    for (size_t i = 0; i < 3; i++) {
        int res = list_popr(l, &addr);
        t_assert(res != 0);
        t_assert_msg(addr == abs((int)i-2), "expected %d, got %d", abs((int)i-2), addr);
    }

    list_destroy(&l);
}

void test_list_insertl(void) {
    List *l = list_new(int);
    int n[3] = {0, 1, 2};
    for (size_t i = 0; i < 3; i++) {
        list_insertl(l, (void *)&n[i]);
        t_assert_msg(list_len(l) == i + 1, "expected %zu, got %zu", i + 1, list_len(l));
    }

    int addr;
    for (size_t i = 0; i < 3; i++) {
        int res = list_popl(l, &addr);
        t_assert(res != 0);
        t_assert_msg(addr == abs((int)i-2), "expected %d, got %d", abs((int)i-2), addr);
    }

    list_destroy(&l);
}

void test_list_len(void) {
    List *l = list_new(int);
    for (size_t i = 0; i < 10; i++) {
        int n = (int)i;
        (i % 2 == 0) ? list_insertr(l, (void *)&n) : list_insertl(l, (void *)&n);
        t_assert_msg(list_len(l) == i + 1, "expected %zu, got %zu", i+1, list_len(l));
    }

    for (size_t i = 10; i > 0; i--) {
        int addr;
        int res = list_popr(l, &addr);
        t_assert(res != 0);
        t_assert_msg(list_len(l) == i - 1, "expected %zu, got %zu", i-1, list_len(l));
    }

    list_destroy(&l);
}

void test_list_data_size(void) {
    List *l = list_new(int);
    t_assert_msg(list_data_size(l) == sizeof(int), "expected %zu, got %zu", list_data_size(l), sizeof(int));
    list_destroy(&l);

    l = list_new(double);
    t_assert_msg(list_data_size(l) == sizeof(double), "expected %zu, got %zu", list_data_size(l), sizeof(double));
    list_destroy(&l);

    struct data {
        int a;
        double b;
        char *c;
        size_t d;
    } t_data;
    l = list_create(sizeof(t_data));
    t_assert_msg(list_data_size(l) == sizeof(t_data), "expected %zu, got %zu", list_data_size(l), sizeof(t_data));
    list_destroy(&l);
}

void test_list_popr(void) {
    List *l = list_new(int);
    for (size_t i = 0; i < 10; i++) {
        int n = (int)i;
        list_insertl(l, (void *)&n);
    }

    for (size_t i = 0; i < 10; i++) {
        int n;
        list_popr(l, (void *)&n);
        t_assert_msg(n == (int)i, "expected %d, got %d", (int)i, n);
    }

    list_destroy(&l);
}

void test_list_popl(void) {
    List *l = list_new(int);
    for (size_t i = 0; i < 100; i++) {
        int n = (int)i;
        list_insertr(l, (void *)&n);
    }

    for (size_t i = 0; i < 100; i++) {
        int n;
        list_popl(l, (void *)&n);
        t_assert_msg(n == (int)i, "expected %d, got %d", (int)i, n);
    }

    list_destroy(&l);
}

void test_list_erase(void) {
    List *l = list_new(int);
    for (size_t i = 0; i < 100; i++) {
        int n = (int)i;
        list_insertl(l, (void *)&n);
    }

    list_erase(l);
    t_assert(list_len(l) == 0);

    list_destroy(&l);
}

void test_list_destroy(void) {
    List *l = list_new(int);
    for (size_t i = 0; i < 100; i++) {
        int n = (int)i;
        list_insertl(l, (void *)&n);
    }

    list_destroy(&l);
    t_assert(l == NULL);
}

void test_list_print(void) {
    List *l = list_new(int);
    printf("[9, 8, 7, 6, 5, 4, 3, 2, 1, 0]\n");
    for (size_t i = 0; i < 10; i++) {
        int n = (int)i;
        list_insertl(l, (void *)&n);
    }
    list_print(l, print_int, ',');
}

void test_list_string(void) {
    List *l = list_create(sizeof(char *));
    char *strings[] = {"hello world", "", "A long time ago in a galaxy far, far away...", "last"};
    for (size_t i = 0; i < 4; i++) {
        list_insertr(l, &strings[i]);
    }

    list_print(l, print_string, ',');

    list_destroy(&l);
}

void test_list_remove(void) {
    List *l = list_create(sizeof(int));
    int data = 8;
    int res = list_remove(l, (void *)&data);
    t_assert(res == -1);

    list_insertl(l, (void *)&data);
    res = list_remove(l, (void *)&data);
    t_assert(res == 0);
    t_assert(list_len(l) == 0);

    list_destroy(&l);
}

void test_list_lookup(void) {
    List *l = list_create(sizeof(int));
    int data = 8;
    bool res = list_lookup(l, (void *)&data);
    t_assert(res == false);

    list_insertl(l, (void *)&data);
    res = list_lookup(l, (void *)&data);
    t_assert(res == true);

    list_destroy(&l);
}

void test_stack_create(void) {
    Stack *s = stack_create(sizeof(double));
    t_assert(s != NULL);
    t_assert(stack_len(s) == 0);

    stack_destroy(&s);
}

void test_stack_len(void) {
    Stack *s = stack_create(sizeof(float));
    for (size_t i = 0; i < 20; i++) {
        float f = (float)i;
        stack_push(s, (void *)&f);
        t_assert_msg(stack_len(s) == i + 1, "expected %zu, got %zu", i+1, stack_len(s));
    }

    for (size_t i = 0; i < 20; i++) {
        float f;
        stack_pop(s, (void *)&f);
        t_assert_msg(stack_len(s) == 19 - i, "expected %zu, got %zu", 19-i, stack_len(s));
    }

    stack_destroy(&s);
}

void test_stack_push_pop(void) {
    Stack *s = stack_create(sizeof(int));
    for (size_t i = 0; i < 20; i++) {
        int n = (int)i;
        stack_push(s, (void *)&n);
    }

    for (size_t i = 0; i < 20; i++) {
        int n;
        stack_pop(s, (void *)&n);
        t_assert_msg(stack_len(s) == 19 - i, "expected %zu, got %zu", 19-i, stack_len(s));
    }

    stack_destroy(&s);
}

void test_stack_destroy(void) {
    Stack *s = stack_create(sizeof(int));
    for (size_t i = 0; i < 20; i++) {
        int n = (int)i;
        stack_push(s, (void *)&n);
    }
    stack_destroy(&s);
    t_assert(s == NULL);
}

// TODO: test printf output
void test_stack_print(void) {
    Stack *s = stack_create(sizeof(int));
    for (size_t i = 0; i < 5; i++) {
        int n = (int)i;
        stack_push(s, (void *)&n);
    }

    printf("[4, 3, 2, 1, 0]\n");
    stack_print(s, print_int, ',');

    stack_destroy(&s);
}

void stack_print(Stack *s, void(*pr_func)(void *), char sep);

int main(void) {
    test_list_new();
    test_list_insertr();
    test_list_insertl();
    test_list_len();
    test_list_data_size();
    test_list_popr();
    test_list_popl();
    test_list_erase();
    test_list_destroy();
    test_list_print();
    test_list_string();
    test_list_lookup();
    test_list_remove();

    test_stack_create();
    test_stack_len();
    test_stack_push_pop();
    test_stack_destroy();
    test_stack_print();
}
