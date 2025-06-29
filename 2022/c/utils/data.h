#ifndef DATA_H
#define DATA_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

// Doubly linked list

typedef struct List List;
typedef struct Iterator Iterator;

/**
 * Must be destroyed by a call to list_destroy.
 * @return NULL if memory allocation fails.
 */
List *list_create(size_t data_size);
#define list_new(type) list_create(sizeof(type))

/**
 * Insertion copies the data.
 */
List *list_insertr(List *l, const void *const data);
List *list_insertl(List *l, const void *const data);

void list_erase(List *l);
void list_erase_data(List *l, void(*del_func)(void *));

/**
 * @param l A pointer to pointer to the list to be destroyed.
 */
void list_destroy(List **l);
void list_destroy_data(List **l, void(*del_func)(void *));

/**
 * @return 1 if the operation is successful or 0 if the list is empty
 */
int list_popr(List *l, void *addr);

/**
 * @return 1 if the operation is successful or 0 if the list is empty
 */
int list_popl(List *l, void *addr);

size_t list_len(List *l);
size_t list_data_size(List *l);

/**
 * @return 0 if the operation is successful or -1 if data is not found
 */
int list_remove(List *l, void *data); 

/**
 * @return true if data is found or false otherwise
 */
bool list_lookup(List *l, const void *const data);

// TODO: to_string
void list_print(List *l, void(*pr_function)(void *), char sep);

Iterator *new_iterator(List *l);
void *iterator_next(Iterator *it);
void iterator_destroy(Iterator **it);

// Stack

typedef struct List Stack;

#define stack_create(width) list_create(width)
#define stack_destroy(s) list_destroy(s)
#define stack_len(s) list_len(s)
#define stack_push(s, data) list_insertl(s, data)
#define stack_pop(s, addr) list_popl(s, addr)
#define stack_print(s, pr_func, sep) list_print(s, pr_func, sep)

// Hash table [string]*void

typedef struct Hashtbl Hashtbl;

typedef struct Entry {
    char *key;
    void *value;
    uint32_t hash;
    struct Entry *next;
} Entry;

typedef struct HashtblIterator {
    const Hashtbl *ht;
    size_t bucket_index;
    Entry *current_entry;
} HashtblIterator;

/**
 * @brief Creates a hashtable.
 *
 * @return a pointer to the newly created hashtable or NULL if allocation failed.
 */
Hashtbl *hashtbl_create(void);

/**
 * @brief Initializes the hash table @htbl. Must be called before any use. The
 *        table contains 2048 slots by default.
 *
 * @param htbl a pointer to a @Hashtbl struct
 * @param destroy function to free dynamically allocated data when
 *        `hashtbl_destroy` is called. 
 * 
 * @return 0 if intialization is successful or -1 otherwise
 */
int hashtbl_init(Hashtbl *ht, void (*destroy)(void *data));

/**
 * @brief Destroys the hash table @htbl.
 *
 * Calls the `destroy`function provided to `hashtbl_init` for each element of
 * the table. No other operations are permitted after calling @hashtbl_destroy.
 */
void hashtbl_destroy(Hashtbl *htbl);

/**
 * @brief Inserts an element in the hash table @htbl.
 *
 * @param data it is the responsability of the caller to create the storage
 *        associated with @value.
 *
 * @return 0 if insertion is successful, -1 otherwise.
 */
int hashtbl_insert(Hashtbl *ht, const char *key, void *value);

/**
 * @brief Provides a non null default value, which is the char '0'.
 *
 * @return a pointer towards the value or NULL if the allocation failed.
 */
void *hashtbl_default_value(void);

/**
 * @brief Destroy function to provide as a callback to hashtbl_init.
 *
 */
void hashtbl_destroy_default_value(void *value);

/**
 * @brief Removes the element matching @key from @ht.
 *
 * @param key the key to remove from @ht.
 *
 * @return 0 if the removing is successful or -1 otherwise.
 */
int hashtbl_remove(Hashtbl *ht, const char *key);

/**
 * @brief Get the current size of the hashtable.
 *
 * @return the number of entries in the hashtable.
 */
size_t hashtbl_size(const Hashtbl *ht);

/**
 * @brief Retrieve the element corresponding to @key in @ht.
 *
 * @param key the key corresponding to the data to be retrieved.
 *
 * @return the value corresponding to @key or NULL if the key was not found. */
void *hashtbl_lookup(const Hashtbl *ht, const char *key);

/**
 * @brief Iterates on each element of the table and executes @callback on each.
 *
 * @param callback the function to be executed on each element of the table.
 */
void hashtbl_foreach(const Hashtbl *ht, void (*callback)(const char *key, void *value));

/**
 * @brief Creates an iterator object on a hash table.
 *
 * @param the hash table.
 */
HashtblIterator *new_hashtblIterator(Hashtbl *ht);

/**
 * @brief Retrieves the next entry in the hash table.
 *
 * @param the iterator.
 */
Entry *hashtblIterator_next(HashtblIterator *it);

/**
 * @brief Destroys the iterator and frees the memory.
 *
 */
void hashtblIterator_destroy(HashtblIterator **it);

#endif // DATA_H
