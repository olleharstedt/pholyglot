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
#include <stddef.h>
#include <stdint.h>

#if !defined(__cplusplus)
	#if (defined(_MSC_VER) && _MSC_VER < 1800) || (!defined(_MSC_VER) && !defined(__STDC_VERSION__))
		#ifndef true
		#define true  (0 == 0)
		#endif
		#ifndef false
		#define false (0 != 0)
		#endif
		typedef unsigned char bool;
	#else
		#include <stdbool.h>
	#endif
#endif

#include <stdio.h>
#include <assert.h>
#include <string.h>

bool is_power_of_two(uintptr_t x) {
	return (x & (x-1)) == 0;
}

uintptr_t align_forward(uintptr_t ptr, size_t align) {
	uintptr_t p, a, modulo;

	assert(is_power_of_two(align));

	p = ptr;
	a = (uintptr_t)align;
	// Same as (p % a) but faster as 'a' is a power of two
	modulo = p & (a-1);

	if (modulo != 0) {
		// If 'p' address is not aligned, push the address to the
		// next value which is aligned
		p += a - modulo;
	}
	return p;
}

#ifndef DEFAULT_ALIGNMENT
#define DEFAULT_ALIGNMENT (2*sizeof(void *))
#endif

typedef struct Arena* Arena;
struct Arena {
    uintptr_t* buf;
    size_t     buf_len;
    size_t     prev_offset; // This will be useful for later on
    size_t     curr_offset;
    Arena      next;        // If no space left, allocate another arena
};

void arena_init(Arena a, uintptr_t* backing_buffer, size_t backing_buffer_length) {
    a->buf = (uintptr_t*) backing_buffer;
    a->buf_len = backing_buffer_length;
    a->curr_offset = 0;
    a->prev_offset = 0;
    a->next        = NULL;
}

uintptr_t* arena_alloc_align(Arena a, size_t size, size_t align) {
    printf("size = %ld\n", size);
    // Align 'curr_offset' forward to the specified alignment
    uintptr_t curr_ptr = (uintptr_t)a->buf + (uintptr_t)a->curr_offset;
    uintptr_t offset = align_forward(curr_ptr, align);
    offset -= (uintptr_t)a->buf; // Change to relative offset

    // Check to see if the backing memory has space left
    if (offset + size <= a->buf_len) {
        printf("offset + size = %ld + %ld = %ld\n", offset, size, offset + size);
        printf("Have space\n");
        uintptr_t* ptr = &a->buf[offset];
        a->prev_offset = offset;
        a->curr_offset = offset+size;

        // Zero new memory by default
        memset(ptr, 0, size);
        return ptr;
    } else {
        printf("Need next\n");
        if (a->next) {
            return arena_alloc_align(a->next, size, align);
        } else {
            size_t new_len = a->buf_len * 2;
            Arena next     = malloc(sizeof(struct Arena));
            arena_init(next, malloc(new_len), new_len);
            a->next        = next;
            return arena_alloc_align(a->next, size, align);
        }
    }
    // Return NULL if the arena is out of memory (or handle differently)
    return NULL;
}

// Because C doesn't have default parameters
uintptr_t* arena_alloc(Arena a, size_t size) {
    return arena_alloc_align(a, size, DEFAULT_ALIGNMENT);
}

void arena_free(Arena a) {
    if (a->next) {
        arena_free(a->next);
    }
    free(a->buf);
}

uintptr_t* arena_resize_align(Arena a, uintptr_t* old_memory, size_t old_size, size_t new_size, size_t align) {
    uintptr_t* old_mem = (uintptr_t*) old_memory;

    assert(is_power_of_two(align));

    if (old_mem == NULL || old_size == 0) {
        return arena_alloc_align(a, new_size, align);
    } else if (a->buf <= old_mem && old_mem < a->buf+a->buf_len) {
        if (a->buf+a->prev_offset == old_mem) {
            a->curr_offset = a->prev_offset + new_size;
            if (new_size > old_size) {
                // Zero the new memory by default
                memset(&a->buf[a->curr_offset], 0, new_size-old_size);
            }
            return old_memory;
        } else {
            void *new_memory = arena_alloc_align(a, new_size, align);
            size_t copy_size = old_size < new_size ? old_size : new_size;
            // Copy across old memory to the new memory
            memmove(new_memory, old_memory, copy_size);
            return new_memory;
        }

    } else {
        assert(0 && "Memory is out of bounds of the buffer in this arena");
        return NULL;
    }

}

// Because C doesn't have default parameters
uintptr_t* arena_resize(Arena a, void *old_memory, size_t old_size, size_t new_size) {
    return arena_resize_align(a, old_memory, old_size, new_size, DEFAULT_ALIGNMENT);
}

void arena_free_all(Arena a) {
    a->curr_offset = 0;
    a->prev_offset = 0;
}

// Extra Features
typedef struct Temp_Arena_Memory Temp_Arena_Memory;
struct Temp_Arena_Memory {
    Arena arena;
    size_t prev_offset;
    size_t curr_offset;
};

Temp_Arena_Memory temp_arena_memory_begin(Arena a) {
    Temp_Arena_Memory temp;
    temp.arena = a;
    temp.prev_offset = a->prev_offset;
    temp.curr_offset = a->curr_offset;
    return temp;
}

void temp_arena_memory_end(Temp_Arena_Memory temp) {
    temp.arena->prev_offset = temp.prev_offset;
    temp.arena->curr_offset = temp.curr_offset;
}

/*
   int main(int argc, char **argv) {
   int i;

   uintptr_t* backing_buffer = malloc(256);
   struct Arena a = {0};
   arena_init(&a, backing_buffer, 256);

   for (i = 0; i < 10; i++) {
   int *x;
   float *f;
   char *str;

// Reset all arena offsets for each loop
arena_free_all(&a);

x = (int *)arena_alloc(&a, sizeof(int));
f = (float *)arena_alloc(&a, sizeof(float));
str = arena_alloc(&a, 10);

 *x = 123;
 *f = 987;
 memmove(str, "Hellope", 7);

 printf("%p: %d\n", x, *x);
 printf("%p: %f\n", f, *f);
 printf("%p: %s\n", str, str);

 str = arena_resize(&a, str, 10, 16);
 memmove(str+7, " world!", 7);
 printf("%p: %s\n", str, str);
 }

 arena_free_all(&a);

 return 0;
 }
 */
