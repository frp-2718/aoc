/*
 * Utility data structures implemented for learning purposes only and not meant
 * to be used in any production environment.
 */

#include <limits.h>
#include <stdbool.h>
#include <stdint.h>
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

// Iterator
struct Iterator {
    List *l;
    ListItem *current;
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
    ListItem *tmp = NULL;
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

void list_erase_data(List *l, void(*del_func)(void *)) {
    ListItem *item = l->first;
    ListItem *tmp = NULL;
    while (item) {
        tmp = item->next;
        del_func(item->data);
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

void list_destroy_data(List **l, void(*del_func)(void *)) {
    list_erase_data(*l, del_func);
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

struct Iterator *new_iterator(List *l) {
    struct Iterator *it = malloc(sizeof(struct Iterator));
    if (!it) return NULL;
    it->l = l;
    it->current = l->first;
    return it;
}

void *iterator_next(struct Iterator *it) {
    void *elem = NULL;
    if (it->current) {
        elem = it->current->data;
        it->current = it->current->next;
    }
    return elem;
}

void iterator_destroy(struct Iterator **it) {
    free(*it);
    *it = NULL;
}

// Hash table
#define HT_DEFAULT_CAPACITY 2048
#define HT_MAX_LOAD 0.7f

typedef struct Hashtbl {
    Entry **buckets;
    size_t capacity;
    size_t size;
    void (*destroy)(void *data);
} Hashtbl;

static uint32_t djb2_hash(const char* key)
{
    uint32_t h = 5381u;
    while (*key) {
        h = ((h << 5) + h) + *key;
        ++key;
    }
    return h;
}

static int hashtbl_resize(Hashtbl *ht, size_t new_capacity) {
    if (!ht || new_capacity == 0) return -1;

    Entry **new_buckets = calloc(new_capacity, sizeof(Entry *));
    if (!new_buckets) return -1;

    for (size_t i = 0; i < ht->capacity; ++i) {
        Entry *entry = ht->buckets[i];
        while (entry) {
            Entry *next = entry->next;
            size_t new_idx = entry->hash % new_capacity;
            entry->next = new_buckets[new_idx];
            new_buckets[new_idx] = entry;
            entry = next;
        }
    }

    free(ht->buckets);
    ht->buckets = new_buckets;
    ht->capacity = new_capacity;

    return 0;
}

Hashtbl *hashtbl_create(void) {
    return malloc(sizeof(Hashtbl));
}

int hashtbl_init(Hashtbl *ht, void (*destroy)(void *data)) {
    if (!ht) return -1;

    ht->buckets = calloc(HT_DEFAULT_CAPACITY, sizeof(Entry *));
    if (!ht->buckets) return -1;

    ht->capacity = HT_DEFAULT_CAPACITY;
    ht->size = 0;
    ht->destroy = destroy;

    return 0;
}

void hashtbl_destroy(Hashtbl *ht) {
    if (!ht) return;

    for (size_t i = 0; i < ht->capacity; ++i) {
        Entry *entry = ht->buckets[i];
        while (entry) {
            Entry *next = entry->next;
            free(entry->key);
            if (ht->destroy) {
                ht->destroy(entry->value);
            }
            free(entry);
            entry = next;
        }
    }
    free(ht->buckets);
    ht->buckets = NULL;
    ht->capacity = 0;
    ht->size = 0;
    ht->destroy = NULL;

    free(ht);
    ht = NULL;
}

int hashtbl_insert(Hashtbl *ht, const char *key, void *value) {
    if (!ht || !key) return -1;

    if (ht->size + 1 > ht->capacity * HT_MAX_LOAD) {
        if (hashtbl_resize(ht, ht->capacity * 2) != 0) {
            return -1;
        }
    }

    uint32_t hash = djb2_hash(key);
    size_t idx = hash % ht->capacity;

    Entry *current = ht->buckets[idx];
    while(current) {
        if (strcmp(current->key, key) == 0) {
            if (ht->destroy) {
                ht->destroy(current->value);
            }
            current->value = value;
            return 0;
        }
        current = current->next;
    }

    Entry *new_entry = malloc(sizeof(Entry));
    if (!new_entry) return -1;

    new_entry->key = strdup(key);
    if (!new_entry->key) {
        free(new_entry);
        return -1;
    }

    new_entry->value = value;
    new_entry->hash = hash;
    new_entry->next = ht->buckets[idx];

    ht->buckets[idx] = new_entry;
    ht->size++;

    return 0;
}

int hashtbl_remove(Hashtbl *ht, const char *key) {
    if (!ht || !key) return -1;

    uint32_t hash = djb2_hash(key);
    size_t idx = hash % ht->capacity;

    Entry *current = ht->buckets[idx];
    Entry *prev = NULL;

    while (current) {
        if (strcmp(current->key, key) == 0) {
            if (prev) {
                prev->next = current->next;
            } else {
                ht->buckets[idx] = current->next;
            }

            free(current->key);
            if (ht->destroy) {
                ht->destroy(current->value);
            }
            free(current);
            ht->size--;

            return 0;
        }
        prev = current;
        current = current->next;
    }

    return -1;
}

void *hashtbl_lookup(const Hashtbl *ht, const char *key) {
    if (!ht || !key) return NULL;

    uint32_t hash = djb2_hash(key);
    size_t idx = hash % ht->capacity;

    Entry *current = ht->buckets[idx];
    while (current) {
        if (strcmp(current->key, key) == 0) {
            return current->value;
        }
        current = current->next;
    }

    return NULL;
}

size_t hashtbl_size(const Hashtbl *ht) {
    return ht->size;
}

void hashtbl_foreach(const Hashtbl *ht, void (*callback)(const char *key, void *value)) {
    if (!ht || !ht->buckets || !callback) return;

    for (size_t i = 0; i < ht->capacity; i++) {
        Entry *entry = ht->buckets[i];
        while (entry) {
            callback(entry->key, entry->value);
            entry = entry->next;
        }
    }
}

HashtblIterator *new_hashtblIterator(Hashtbl *ht) {
    HashtblIterator *it = malloc(sizeof(HashtblIterator));
    if (!it) return NULL;
    it->ht = ht;
    it->bucket_index = -1;
    it->current_entry = NULL;
    return it;
}

Entry *hashtblIterator_next(HashtblIterator *it) {
    if (!it || !it->ht) return NULL;

    if (it->current_entry && it->current_entry->next) {
        it->current_entry = it->current_entry->next;
        return it->current_entry;
    }

    while (++it->bucket_index < it->ht->capacity) {
        Entry *entry = it->ht->buckets[it->bucket_index];
        if (entry) {
            it->current_entry = entry;
            return it->current_entry;
        }
    }

    return NULL;
}

void hashtblIterator_destroy(HashtblIterator **it) {
    if (it && *it) {
        free(*it);
        *it = NULL;
    }
}

void *hashtbl_default_value(void) {
    char *p = malloc(sizeof(char));
    if (!p) return NULL;
    *p = '0';
    return (void *)p;
}

void hashtbl_destroy_default_value(void *value) {
    if (value) {
        free(value);
        value = NULL;
    }
}
