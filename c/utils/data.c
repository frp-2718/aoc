#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "data.h"

// List
typedef struct ListItem {
    void *data;
    struct ListItem *next;
    struct ListItem *prev;
} ListItem;

struct List {
    ListItem *first;
    ListItem *last;
    size_t len;
    size_t data_size;
};

List *list_create(size_t data_size) {
    List *l = malloc(sizeof(List));
    if (!l) return NULL;

    l->first = NULL;
    l->last = NULL;
    l->len = 0;
    l->data_size = data_size;

    return l;
}

size_t list_len(List *l) {
    if (!l) return 0;
    return l->len;
}

size_t list_data_size(List *l) {
    if (!l) return 0;
    return l->data_size;
}

List *list_insertr(List *l, void *data) {
    if (!l) return NULL;

    ListItem *item = malloc(sizeof(ListItem));
    if (!item) return NULL;

    item->data = malloc(l->data_size);
    if (!item->data) return NULL;

    item->next = NULL;
    item->prev = l->last;
    memcpy(item->data, data, l->data_size);
    if (l->last)
        l->last->next = item;
    l->last = item;
    if (!l->first)
        l->first = item;
    l->len++;
    return l;
}

List *list_insertl(List *l, void *data) {
    if (!l) return NULL;

    ListItem *item = malloc(sizeof(ListItem));
    if (!item) return NULL;

    item->data = malloc(l->data_size);
    if (!item->data) return NULL;

    item->prev = NULL;
    item->next = l->first;
    memcpy(item->data, data, l->data_size);
    if (l->first)
        l->first->prev = item;
    l->first = item;
    if (!l->last)
        l->last = item;
    l->len++;
    return l;
}

void list_erase(List *l) {
    ListItem *item = l->first;
    ListItem *tmp = NULL;;
    while (item) {
        tmp = item->next;
        free(item->data);
        free(item);
        item = tmp;
    }
    l->first = NULL;
    l->last = NULL;
    l->len = 0;
}

void list_destroy(List **l) {
    list_erase(*l);
    free(*l);
    *l = NULL;
}

void list_print(List *l, void(*pr_func)(void *), char sep) {
    printf("[");
    ListItem *current = l->first;
    while (current) {
        void *element = current->data;
        pr_func(element);
        current = current->next;
        if (current) printf("%c ", sep);
    }
    printf("]\n");
}

int list_popr(List *l, void *addr) {
    if (!l || !l->last) {
        return 0;
    }
    ListItem *item = l->last;
    memcpy(addr, item->data, l->data_size);
    l->last = item->prev;
    if (l->last)
        l->last->next = NULL;
    else
        l->first = NULL;

    free(item->data);
    free(item);
    l->len--;
    return 1;
}

int list_popl(List *l, void *addr) {
    if (!l || !l->first) {
        return 0;
    }
    ListItem *item = l->first;
    memcpy(addr, item->data, l->data_size);
    l->first = item->next;
    if (l->first)
        l->first->prev = NULL;
    else
        l->last = NULL;

    free(item->data);
    free(item);
    l->len--;
    return 1;
}
