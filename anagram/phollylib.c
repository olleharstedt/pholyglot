#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>
#include <string.h>
#include <stddef.h>
#include <stdbool.h>
#include <gc.h>
#ifndef DEFAULT_ALIGNMENT
#define DEFAULT_ALIGNMENT (2*sizeof(void *))
#endif
// #define float double
// #define int long // TODO: This can cause some problems with COMPARE_MIXED on _Bool, since false is an int
// TODO: static_assert(sizeof(long) == sizeof(double) == sizeof(uintptr_t));
#define class struct
#define __PHP__ 0
#define new(x, m) x ## __constructor((x) m.alloc(m.arena, sizeof(struct x)), m)
#define clone(var, x, m) x ## __clone(var, m)
#define array(...) {__VA_ARGS__}
#define array_make(type, i, ...) {.thing = (type[]) array(__VA_ARGS__), .length = i}
#define array_get(type, arr, i) ((type*) arr.thing)[i]
#define count(x) x.length
#define pprintf printf
#define STDERR stderr
#define STDOUT stdout

// Memory system
struct mem {
    uintptr_t* (*alloc) (void* a, size_t size);
    void* arena;
};
struct mem arena_mem = {0};
void* gc_malloc(void* throw_away, size_t size)
{
    return GC_MALLOC(size);
}
void* heap_malloc(void* throw_away, size_t size)
{
    return malloc(size);
}
struct mem gc_mem = {.alloc = &gc_malloc, .arena = NULL};
struct mem heap_mem = {.alloc = &heap_malloc, .arena = NULL};
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
        // TODO: Hard-coded malloc should use PH_SET_ALLOC
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

    struct mem mem;
};

void SplDoublyLinkedList__push(SplDoublyLinkedList self, uintptr_t* item)
{
    if (self->item == NULL) {
        self->item = item;
    } else {
        SplDoublyLinkedList n = self->mem.alloc(self->mem.arena, sizeof(struct SplDoublyLinkedList));
        if (n == NULL) {
            printf("No mem\n");
        }
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

void SplDoublyLinkedList__next(SplDoublyLinkedList self)
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

SplDoublyLinkedList SplDoublyLinkedList__constructor(SplDoublyLinkedList self, struct mem m)
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

    self->mem = m;

    return self;
}

// Arena allocator
// @see https://www.gingerbill.org/article/2019/02/08/memory-allocation-strategies-002/

_Bool is_power_of_two(uintptr_t x) {
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

typedef struct Arena* Arena;
struct Arena {
    unsigned char* buf;
    size_t     buf_len;
    size_t     prev_offset; // This will be useful for later on
    size_t     curr_offset;
    Arena      next;        // If no space left, allocate another arena
};

void arena_init(Arena a, uintptr_t* backing_buffer, size_t buf_len) {
    a->buf = (unsigned char*) backing_buffer;
    void* ptr = &a->buf[buf_len];
    a->buf_len = buf_len;
    a->curr_offset = 0;
    a->prev_offset = 0;
    a->next        = NULL;
    memset(a->buf, 0, buf_len);
}

void* arena_alloc_align(Arena a, size_t size, size_t align) {
    // Align 'curr_offset' forward to the specified alignment
    uintptr_t curr_ptr = (uintptr_t)a->buf + (uintptr_t)a->curr_offset;
    uintptr_t offset = align_forward(curr_ptr, align);
    offset -= (uintptr_t)a->buf; // Change to relative offset

    // Check to see if the backing memory has space left
    if (offset + size <= a->buf_len) {
        unsigned char* ptr = &a->buf[offset];
        a->prev_offset = offset;
        a->curr_offset = offset+size;

        // Zero new memory by default
        memset(ptr, 0, size);
        return ptr;
    } else {
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
    free(a);
}

// Must be used as header for all internal types, to support usage of both mixed and others.
// Example: substr(file_get_contents("filename")) vs substr("moo")
enum type
{
    MIXED_STRING = 0,
    MIXED_BOOL   = 2,
    SMART_STRING = 1
    // TODO: Need to extend this indefinitely for each type in the system? Or only core types?
};

typedef struct _Unknown* Unknown;
struct _Unknown
{
    enum type t;
};

// PHP string system

typedef struct _smartstr* smartstr;
struct _smartstr
{
    enum type t;
    char* str;
    size_t len;
    // TODO: Add size of current buffer?
};

// PHP mixed result type

typedef struct _Mixed Mixed;
struct _Mixed
{
    enum type t;
    union {
        smartstr s;
        bool  b;
    };
    // TODO: Field for custom types
};

#define COMPARE_MIXED(mixed, val) _Generic(val ,\
    int : (mixed.t == MIXED_BOOL && mixed.b == val),\
    char*: (mixed.t == MIXED_STRING && strncmp(mixed.s->str, val, mixed.s->len) == 0)\
    )


#define PH_SET_ALLOC(m) uintptr_t* (*alloc) (void* a, size_t size); if (m) alloc = m->alloc; else alloc = gc_malloc;
#define PH_GET_ARENA(m) m == NULL ? NULL : m->arena
#define PH_ALLOC(s) alloc(PH_GET_ARENA(m), sizeof(s))

// TODO: Which memory strategy to use?
struct _smartstr* ph_smartstr_new(const char* s, struct mem* m)
{
    PH_SET_ALLOC(m);
    smartstr result;

    result = PH_ALLOC(*result);
    result->len = strlen(s);
    result->str = alloc(NULL, result->len);
    result->str = strcpy(result->str, s);

    return result;
}

// Some copy-paste from php-src
#define zend_long long
#define ZSTR_LEN(str) (str)->len
smartstr substr(Unknown _str, long f, int length, struct mem* m)
{
    PH_SET_ALLOC(m);
	long l = 0;
	bool len_is_null = 1;
    Mixed mixed;
    smartstr str;

    switch (_str->t) {
        case SMART_STRING:
            str = (smartstr) _str;
            break;
        case MIXED_STRING:
            str = ((Mixed*) _str)->s;
            break;
        default:
            exit(123);
            break;
    }

	if (f < 0) {
		/* if "from" position is negative, count start position from the end
		 * of the string
		 */
		if (-(size_t)f > ZSTR_LEN(str)) {
			f = 0;
		} else {
			f = (long) ZSTR_LEN(str) + f;
		}
	} else if ((size_t)f > ZSTR_LEN(str)) {
        // Return empty string
	    return ph_smartstr_new("", m);
	}

	if (!len_is_null) {
		if (l < 0) {
			/* if "length" position is negative, set it to the length
			 * needed to stop that many chars from the end of the string
			 */
			if (-(size_t)l > ZSTR_LEN(str) - (size_t)f) {
				l = 0;
			} else {
				l = (zend_long)ZSTR_LEN(str) - f + l;
			}
		} else if ((size_t)l > ZSTR_LEN(str) - (size_t)f) {
			l = (zend_long)ZSTR_LEN(str) - f;
		}
	} else {
		l = (zend_long)ZSTR_LEN(str) - f;
	}

	if (l == ZSTR_LEN(str)) {
        // TODO: Assuming str is null-terminated?
        str->str[50] = '\0';
        return ph_smartstr_new(str->str, m);
	} else {
        //return ph_smartstr_copy(str, f, l, m);
		//RETURN_STRINGL_FAST(ZSTR_VAL(str) + f, l);
	}
}

struct _smartstr* ph_smartstr_copy(struct _smartstr* str, long offset, int length, struct mem* m)
{
    PH_SET_ALLOC(m);
    if (length < 0) {
        return NULL;
    }

    smartstr result = PH_ALLOC(*result);
    size_t new_length = length - offset;
    result->str = PH_ALLOC(new_length);
    char* tmp = str->str[offset];
    strncpy(result->str, str->str, length);
    return result;
}


// TODO: To free a string depends on how it was alloced - arena, gc, stack, or heap.
void ph_smartstr_free(smartstr s)
{
    if (s == NULL) {
        return;
    }
    if (s->str) {
        free(s->str);
    }
    if (s) {
        free(s);
    }
}

#define OP_EQUALS ==
#define OP_PLUS +

// Prefix functions with ph_
void ph_free_mixed(struct _Mixed* m)
{
    // Mixed should always be stack allocated.
    switch (m->t) {
        case MIXED_STRING:
            free(m->s->str);
            free(m->s);
            break;
        case MIXED_BOOL:
            // Nothing to do.
            break;
    }
}

// PHP std lib functions

/**
 * @see https://www.php.net/manual/en/function.file-get-contents.php
 * @see https://stackoverflow.com/questions/174531/how-to-read-the-content-of-a-file-to-a-string-in-c
 * @see https://www.kernel.org/doc/html/v4.10/process/coding-style.html#centralized-exiting-of-functions
 *
 * IRC:
 *   14:10 < pekster> Each block of stuff to do looks more like: if (<allocation and it failed>) { record_error(ENUM_REASON); cleanup(ENUM_REASON); goto err; }
 *   14:20 < pekster> Your preference for unnecessay information hiding with opaque typedefs does not make your code very readable.
 *   14:23 < pekster> To a point it boils down to style, but the Linux (kernel) style guide would not approve of what you've done:
 *                    https://www.kernel.org/doc/html/latest/process/coding-style.html#typedefs
 */
struct _Mixed file_get_contents(struct _smartstr* filename)
{
    // TODO: Custom memory alloc here?

    fprintf(stderr, "file_get_contents\n");
    FILE * f = fopen(filename->str, "rb");
    struct _smartstr* s;
    struct _Mixed return_value;
    if (f) {
        int res = fseek(f, 0, SEEK_END);
        if (res == -1) {
            fprintf(stderr, "SEEK_END res == -1\n");
            goto return_false;
        }
        s = malloc(sizeof(*s));
        s->len = ftell(f);
        res = fseek(f, 0, SEEK_SET);
        if (res == -1) {
            fprintf(stderr, "SEEK_SET res == -1\n");
            goto return_false;
        }
        s->str = malloc(s->len);
        if (s->str) {
            fprintf(stderr, "before fread\n");
            long chunk = fread(s->str, 1, s->len, f);
            fprintf(stdout, "chunk = %ld\n", chunk);
            if (chunk != s->len) {
                goto return_false;
            }
            if (ferror(f)) {
                goto return_false;
            } else {
                // Success case
                return (Mixed) {.t = MIXED_STRING, .s = s};
            }
        } else {
            goto return_false;
        }
        fclose (f);
    } else {
        fprintf(stderr, "Could not open file\n");
        goto return_false;
    }

return_false:
    return_value = (Mixed) {.t = MIXED_BOOL, .b = false};
cleanup:
    if (f) {
        fclose(f);
    }
    ph_smartstr_free(s);
    return return_value;
}
