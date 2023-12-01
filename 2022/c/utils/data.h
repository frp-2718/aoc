#ifndef DATA_H
#define DATA_H

#include <stdbool.h>
#include <stddef.h>

// Doubly linked list

typedef struct List List;

List *list_create(size_t data_size);
#define list_new(type) list_create(sizeof(type))

List *list_insertr(List *l, const void *const data);
List *list_insertl(List *l, const void *const data);
void list_erase(List *l);
void list_destroy(List **l);

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

// Stack

typedef struct List Stack;

#define stack_create(width) list_create(width)
#define stack_destroy(s) list_destroy(s)
#define stack_len(s) list_len(s)
#define stack_push(s, data) list_insertl(s, data)
#define stack_pop(s, addr) list_popl(s, addr)
#define stack_print(s, pr_func, sep) list_print(s, pr_func, sep)

// Hash table

typedef struct Hashtbl Hashtbl;

/**
 * @brief Initializes the hash table @htbl. Must be called before any use.
 *
 * @param htbl a pointer to a @Hashtbl struct
 * @param buckets the number of slots in the hash table
 * @param data_size size of the data stored in the table
 * @param h user-defined hash function
 * @param match user-defined function to determine whether two keys match
 * @param destroy function to free dynamically allocated data when
 *        `hashtbl_destroy` is called. Should be set to `NULL` for a hash table
 *        containing data that should not be freed
 * 
 * @return 0 if intialization is successful or -1 otherwise
 */
int hashtbl_init(Hashtbl *htbl, int buckets, size_t data_size,
        int (*h)(const void * key),
        bool (*match)(const void *key1, const void *key2, size_t size),
        void (*destroy)(void *data));

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
 * @param data it is the responsability of the caller to manage the storage
 *        associated with @data
 *
 * @return 0 if insertion is successful, 1 if the element is already in the
 *         table or -1 otherwise.
 */
int hashtbl_insert(Hashtbl *htbl, void *data);

/**
 * @brief Removes the element matching @data from @htbl.
 *
 * @param data it is the responsability of the caller to manage the storage
 *             associated with the data
 *
 * @return 0 if the removing is successful or -1 otherwise
 */
int hashtbl_remove(Hashtbl *htbl, void **data);

/**
 * @brief Determines if an element of @htbl matches @data.
 *
 * @param data if a match is found, points to the matching data in the table
 *
 * @return true if the element is found or false otherwise
 */
bool hashtbl_lookup(const Hashtbl *htbl, void **data);

/**
 * @brief Macro that evaluates to the number of elements in @htbl
 *
 * @return the number of elements in the hash table.
 */
#define hashtbl_size(htbl) ((htbl)->size)

#endif // DATA_H
