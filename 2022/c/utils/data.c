#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "data.h"

#define STR_CHUNK 1024

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

static ListItem *list_search(List *l, const void *const data) {
    if (l->len == 0) {
        return NULL;
    }
    ListItem *current = l->first;
    while (current) {
        if (memcmp(data, current->data, l->data_size) == 0) {
            return current;
        }
        current = current->next;
    }
    return NULL;
}

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

List *list_insertr(List *l, const void *const data) {
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

List *list_insertl(List *l, const void *const data) {
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

int list_remove(List *l, void *data) {
    ListItem *elem = list_search(l, data);
    if (elem == NULL)
        return -1;

    if (elem->prev != NULL) {
        elem->prev->next = elem->next;
    } else {
        if (elem->next) elem->next->prev = NULL;
        l->first = elem->next;
    }

    if (elem->next != NULL) {
        elem->next->prev = elem->prev;
    } else {
        if (elem->prev) elem->prev->next = NULL;
        l->last = elem->prev;
    }

    l->len--;

    free(elem->data);
    free(elem);
    return 0;
}

bool list_lookup(List *l, const void *const data) {
    ListItem *res = list_search(l, data);
    return res ? true : false;
}

// Hash table
typedef struct Hashtbl {
    size_t buckets;
    int (*h)(const void *key);
    bool (*match)(const void *key1, const void *key2, size_t size);
    void (*destroy)(void *data);
    size_t size;
    size_t data_size;
    List **table;
} Hashtbl;

int hashtbl_init(Hashtbl *htbl, int buckets,
        size_t data_size,
        int (*h)(const void *key),
        bool (*match)(const void *key1, const void *key2, size_t size),
        void (*destroy)(void *data)) {
    if ((htbl->table = malloc(buckets * sizeof(List *))) == NULL)
        return -1;

    htbl->buckets = buckets;
    for (size_t i = 0; i < htbl->buckets; i++) {
        htbl->table[i] = list_create(data_size);
    }

    htbl->h = h;
    htbl->match = match;
    htbl->destroy = destroy;

    htbl->size = 0;
    htbl->data_size = data_size;

    return 0;
}

void hashtbl_destroy(Hashtbl *htbl) {
    for (size_t i = 0; i < htbl->buckets; i++) {
        list_destroy(&htbl->table[i]);
    }
    free(htbl->table);
    memset(htbl, 0, sizeof(Hashtbl));
}

int hashtbl_insert(Hashtbl *htbl, void *data) {
    void *temp = (void *)data;
    if (hashtbl_lookup(htbl, &temp))
        return 1;

    int bucket = htbl->h(data) % htbl->buckets;

    htbl->table[bucket] = list_insertl(htbl->table[bucket], data);
    if (!htbl->table[bucket])
        return -1;

    htbl->size++;

    return 0;
}

int hashtbl_remove(Hashtbl *htbl, void **data) {
    int bucket = htbl->h(*data) % htbl->buckets;
    if (list_remove(htbl->table[bucket], *data) == 0) {
        htbl->size--;
        return 0;
    }
    return -1;
}

bool hashtbl_lookup(const Hashtbl *htbl, void **data) {
    int bucket = htbl->h(*data) % htbl->buckets;
    return list_lookup(htbl->table[bucket], data);
}
