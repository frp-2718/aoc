#ifndef DATA_H
#define DATA_H

#include <stdbool.h>
#include <stddef.h>

// List
typedef struct List List;

List *list_create(size_t data_size);

#define list_new(type) list_create(sizeof(type))

List *list_insertr(List *l, void *data);
List *list_insertl(List *l, void *data);
void list_erase(List *l);
void list_destroy(List **l);
void list_print(List *l, void(*pr_function)(void *), char sep);
int list_popr(List *l, void *addr);
int list_popl(List *l, void *addr);
size_t list_len(List *l);
size_t list_data_size(List *l);

// Stack

typedef struct List Stack;

#define stack_create(width) list_create(width)
#define stack_destroy(s) list_destroy(s)
#define stack_len(s) list_len(s)
#define stack_push(s, data) list_insertl(s, data)
#define stack_pop(s, addr) list_popl(s, addr)
#define stack_print(s, pr_func, sep) list_print(s, pr_func, sep)

#endif // DATA_H
