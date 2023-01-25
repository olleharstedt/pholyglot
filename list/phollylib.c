#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#define float double
#define int long
// TODO:  static_assert(sizeof(long) == sizeof(double) == sizeof(uintptr_t));
#define class struct
#define __PHP__ 0
#define new(x) x ## __constructor(malloc(sizeof(struct x)))
#define array(...) {__VA_ARGS__}
#define array_make(type, i, ...) {.thing = (type[]) array(__VA_ARGS__), .length = i}
#define array_get(type, arr, i) ((type*) arr.thing)[i]
#define count(x) x.length
#define pprintf printf
typedef struct array array;
struct array {
    uintptr_t* thing;
    size_t length;
};
array array_slice(array old, int offset)
{
    size_t new_length = old.length - offset;
    if (new_length < 1) {
        return (array) {.length = 0, .thing = NULL};
    }
    array new = {
        .length = new_length,
        .thing = malloc(sizeof(uintptr_t) * new_length)
    };
    size_t j = 0;
    for (size_t i = offset; i < old.length; i++) {
        new.thing[j] = old.thing[i]; j++;
    }
    return new;
}
typedef struct SplDoublyLinkedList* SplDoublyLinkedList;
struct SplDoublyLinkedList {
    uintptr_t* item;
    struct SplDoublyLinkedList* next_node;
    struct SplDoublyLinkedList* last;
    struct SplDoublyLinkedList* current_node;

    void (*push) (SplDoublyLinkedList self, uintptr_t* item);
    void (*next) (SplDoublyLinkedList self);
    uintptr_t* (*current) (SplDoublyLinkedList self);
    _Bool (*valid) (SplDoublyLinkedList self);
    void (*rewind) (SplDoublyLinkedList self);
};

void SplDoublyLinkedList__push(SplDoublyLinkedList self, uintptr_t* item)
{
    if (self->item == NULL) {
        self->item = item;
    } else {
        SplDoublyLinkedList n = malloc(sizeof(struct SplDoublyLinkedList));
        n->item = item;

        SplDoublyLinkedList current = self;
        while (current->next_node != NULL) {
            current = current->next_node;
        }

        current->next_node = n;
        current->next_node->next_node = NULL;
    }
}

uintptr_t* SplDoublyLinkedList__current(SplDoublyLinkedList self)
{
    return self->current_node->item;
}

SplDoublyLinkedList SplDoublyLinkedList__next(SplDoublyLinkedList self)
{
    if (self->current_node) {
        self->current_node = self->current_node->next_node;
    }
}

_Bool SplDoublyLinkedList__valid(SplDoublyLinkedList self)
{
    return self->current_node != NULL;
}

void SplDoublyLinkedList__rewind(SplDoublyLinkedList self)
{
    self->current_node = self;
}

SplDoublyLinkedList SplDoublyLinkedList__constructor(SplDoublyLinkedList self)
{
    self->push         = &SplDoublyLinkedList__push;
    self->current      = &SplDoublyLinkedList__current;
    self->next         = &SplDoublyLinkedList__next;
    self->valid        = &SplDoublyLinkedList__valid;
    self->rewind       = &SplDoublyLinkedList__rewind;

    self->item         = NULL;
    self->last         = NULL;
    self->next_node    = NULL;
    self->current_node = self;
    return self;
}

// Arena allocator
// @see https://www.gingerbill.org/article/2019/02/08/memory-allocation-strategies-002/
typedef struct arena* arena;
struct arena {
    uintptr_t* chunk;
    uintptr_t* next_chunk;
    size_t size;
    size_t offset;
};

arena arena_init(size_t size)
{
    arena a       = malloc(sizeof(struct arena));
    a->chunk      = malloc(size);
    a->next_chunk = NULL;
    a->size       = size;
    a->offset     = 0;
    return a;
}

uintptr_t* arena_alloc(arena a, size_t size)
{
    if (a->offset + size > a->size) {
        printf("here");
        exit(123);
    }
    //printf("offset = %ld\n", a->offset);
    uintptr_t* ptr = &(a->chunk[a->offset]);
    a->offset = a->offset + size;
    memset(ptr, 0, size);
    return ptr;
}

void arena_free(arena a)
{
    free(a->chunk);
    // TODO: For each next_chunk, free
    free(a);
}
