#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>
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

#define GET_STRING(s) s->str

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

/*
#define CREATE_MIXED(x) _Generic(x,\
    Mixed: x,\
    smartstr: (Mixed) {.t = MIXED_STRING, .s = x},\
    char*: (Mixed) {.t = MIXED_STRING, .s = ph_smartstr_new(x, NULL)}\
    )
*/

#define COMPARE_MIXED(mixed, val) _Generic(val ,\
    int : (mixed.t == MIXED_BOOL && mixed.b == val),\
    char*: (mixed.t == MIXED_STRING && strncmp(mixed.s->str, val, mixed.s->len) == 0)\
    )

#define PH_SET_ALLOC(m) uintptr_t* (*alloc) (void* a, size_t size); if (m) alloc = m->alloc; else alloc = gc_malloc;
#define PH_GET_ARENA(m) m == NULL ? NULL : m->arena
#define PH_ALLOC(s) alloc(PH_GET_ARENA(m), sizeof(s))
#define PH_ABORT(s) fprintf(stderr, "FATAL INTERNAL ERROR: %s\n", s); exit(123);
#define PH_DEBUG 0
#if PH_DEBUG
#define ERROR_LOG(s) fprintf(stderr, "ERROR_LOG: %s\n", (s))
#else
#define ERROR_LOG(s) 
#endif

// TODO: Which memory strategy to use?
struct _smartstr* ph_smartstr_new(const char* s, struct mem* m)
{
    PH_SET_ALLOC(m);
    smartstr result;

    if (s == NULL) {
        //PH_ABORT("ph_smartstr_new: s is null");
    }

    result = PH_ALLOC(*result);
    result->len = strlen(s);
    result->str = alloc(NULL, result->len);
    result->str = strcpy(result->str, s);

    return result;
}

// This is used, assuming it's already known that Mixed is indeed a packaged smartstr.
#define GET_MIXED_STRING(x) _Generic(x,\
    smartstr: x,\
    Mixed: x.s\
    )

struct _smartstr* ph_smartstr_copy(struct _smartstr* str, long offset, int length, struct mem* m)
{
    PH_SET_ALLOC(m);
    smartstr result;
    char* tmp;

    ERROR_LOG(sprintf("offset = %ld\n", offset));
    ERROR_LOG(sprintf("length = %d\n", length));

    if (length < 0) {
        ERROR_LOG("length < 0");
        return NULL;
    }

    result = PH_ALLOC(*result);
    result->str = PH_ALLOC(length);
    tmp = &str->str[offset];
    strncpy(result->str, tmp, length);
    return result;
}

// Some copy-paste from php-src
#define zend_long long
#define ZSTR_LEN(str) (str)->len
#define ZSTR_VAL(s) (s)->str
#define ZEND_LONG_MAX INT64_MAX
smartstr substr(smartstr _str, long f, int length, struct mem* m)
{
    ERROR_LOG("substr");
    PH_SET_ALLOC(m);
	long l = length;
	bool len_is_null = 0;
    Mixed* mixed;
    //smartstr str = PH_ALLOC(*str);
    smartstr str = _str;

    /*
    switch (_str->t) {
        case SMART_STRING:
            ERROR_LOG("_str->t = SMART_STRING");
            str = (smartstr) _str;
            break;
        case MIXED_STRING:
            ERROR_LOG("_str->t = MIXED_STRING");
            mixed = (Mixed*) _str;
            ERROR_LOG(mixed->s->str);
            str = ((Mixed*) _str)->s;
            break;
        default:
            PH_ABORT("substr: Invalid type");
            break;
    }
    */

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
        ERROR_LOG("Return empty string");
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
        ERROR_LOG("Return exact copy");
        // TODO: Assuming str is null-terminated?
        ERROR_LOG(sprintf("str->str = %.5s\n", str->str));
        //return ph_smartstr_new(str->str, m);
        return str;
	} else {
        ERROR_LOG("l not equal ZSTR_LEN");
        return ph_smartstr_copy(str, f, l, m);
		//RETURN_STRINGL_FAST(ZSTR_VAL(str) + f, l);
	}
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

    ERROR_LOG("file_get_contents");
    FILE * f = fopen(filename->str, "rb");
    struct _smartstr* s;
    struct _Mixed return_value;
    if (f) {
        int res = fseek(f, 0, SEEK_END);
        if (res == -1) {
            ERROR_LOG("SEEK_END res == -1\n");
            goto return_false;
        }
        s = malloc(sizeof(*s));
        s->len = ftell(f);
        res = fseek(f, 0, SEEK_SET);
        if (res == -1) {
            ERROR_LOG("SEEK_SET res == -1\n");
            goto return_false;
        }
        s->str = malloc(s->len);
        if (s->str) {
            ERROR_LOG("before fread\n");
            long chunk = fread(s->str, 1, s->len, f);
            ERROR_LOG(sprintf("chunk = %ld\n", chunk));
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

typedef struct _Array* Array;
struct _Array
{
    smartstr* strings;
    // Current items in strings
    size_t len;
    // Current total size
    size_t size;
};

#define UNEXPECTED(x) x
#define EXPECTED(x) x
#define ZEND_FASTCALL 
void zend_memnstr_ex_pre(unsigned int td[], const char *needle, size_t needle_len, int reverse) /* {{{ */ {
	int i;

	for (i = 0; i < 256; i++) {
		td[i] = needle_len + 1;
	}

	if (reverse) {
		for (i = needle_len - 1; i >= 0; i--) {
			td[(unsigned char)needle[i]] = i + 1;
		}
	} else {
		size_t i;

		for (i = 0; i < needle_len; i++) {
			td[(unsigned char)needle[i]] = (int)needle_len - i;
		}
	}
}
const char* ZEND_FASTCALL zend_memnstr_ex(const char *haystack, const char *needle, size_t needle_len, const char *end) /* {{{ */
{
	unsigned int td[256];
	size_t i;
	const char *p;

	if (needle_len == 0 || (end - haystack) < needle_len) {
		return NULL;
	}

	zend_memnstr_ex_pre(td, needle, needle_len, 0);

	p = haystack;
	end -= needle_len;

	while (p <= end) {
		for (i = 0; i < needle_len; i++) {
			if (needle[i] != p[i]) {
				break;
			}
		}
		if (i == needle_len) {
			return p;
		}
		if (UNEXPECTED(p == end)) {
			return NULL;
		}
		p += td[(unsigned char)(p[needle_len])];
	}

	return NULL;
}
const char * zend_memnstr(const char *haystack, const char *needle, size_t needle_len, const char *end)
{
	const char *p = haystack;
	size_t off_s;

	//ZEND_ASSERT(end >= p);

	if (needle_len == 1) {
		return (const char *)memchr(p, *needle, (end-p));
	} else if (UNEXPECTED(needle_len == 0)) {
		return p;
	}

	off_s = (size_t)(end - p);

	if (needle_len > off_s) {
		return NULL;
	}

	if (EXPECTED(off_s < 1024 || needle_len < 9)) {	/* glibc memchr is faster when needle is too short */
		const char ne = needle[needle_len-1];
		end -= needle_len;

		while (p <= end) {
			if ((p = (const char *)memchr(p, *needle, (end-p+1)))) {
				if (ne == p[needle_len-1] && !memcmp(needle+1, p+1, needle_len-2)) {
					return p;
				}
			} else {
				return NULL;
			}
			p++;
		}

        return NULL;
    } else {
        return zend_memnstr_ex(haystack, needle, needle_len, end);
    }
}
smartstr zend_string_alloc(size_t len, bool persistent)
{
    ERROR_LOG(sprintf("len = %ld\n", len));
    smartstr ret = malloc(sizeof(*ret));
    if (ret == NULL) {
        PH_ABORT("zend_string_alloc: Could not malloc");
    }
    //GC_SET_REFCOUNT(ret, 1);
    //GC_TYPE_INFO(ret) = GC_STRING | ((persistent ? IS_STR_PERSISTENT : 0) << GC_FLAGS_SHIFT);
    //ZSTR_H(ret) = 0;
    ZSTR_LEN(ret) = len;
    return ret;
}
smartstr zend_string_init(const char *str, size_t len, bool persistent)
{
    smartstr ret = zend_string_alloc(len, persistent);
    ERROR_LOG(sprintf("zend_string_init: len = %ld\n", len));
    ERROR_LOG(sprintf("zend_string_init: str = %s\n", str));
    strncpy(ZSTR_VAL(ret), str, len);
    ZSTR_VAL(ret)[len] = '\0';
    return ret;
}
smartstr zend_string_init_fast(const char *str, size_t len)
{
    if (len > 1) {
        return zend_string_init(str, len, 0);
    } else if (len == 0) {
        return NULL;
    } else /* if (len == 1) */ {
        return zend_string_init(str, 1, 0);
    }
}


#define php_memnstr zend_memnstr
Array explode(smartstr delim, smartstr str)
{
	const char *p1 = ZSTR_VAL(str);
	const char *endp = ZSTR_VAL(str) + ZSTR_LEN(str);
	const char *p2 = php_memnstr(ZSTR_VAL(str), ZSTR_VAL(delim), ZSTR_LEN(delim), endp);
    zend_long limit = ZEND_LONG_MAX; /* No limit */
    smartstr tmp = malloc(sizeof(*tmp));
    Array arr = malloc(sizeof(*arr));
    arr->strings = malloc(sizeof(uintptr_t) * 10);
    arr->size = 10;

	if (p2 == NULL) {
		//ZVAL_STR_COPY(&tmp, str);
        size_t len = strlen(str->str);
        tmp->str = malloc(strlen(str->str));
        tmp->len = len;
        strncpy(tmp->str, str->str, len);
        arr->strings[0] = tmp;
        arr->len = 1;
        return arr;
		//zend_hash_next_index_insert_new(Z_ARRVAL_P(return_value), &tmp);
	} else {
		//zend_hash_real_init_packed(Z_ARRVAL_P(return_value));
		//ZEND_HASH_FILL_PACKED(Z_ARRVAL_P(return_value)) {
        size_t j = 0;
			do {
                if (arr->len >= arr->size) {
                    smartstr* tmp = realloc(arr->strings, arr->size * 2);
                    for (size_t i = 0; i < arr->len; i++)
                        tmp[i] = arr->strings[i];
                    free(arr->strings);
                    arr->strings = tmp;
                    arr->size = arr->size * 2;
                }
				//ZEND_HASH_FILL_GROW();
				arr->strings[j] = zend_string_init_fast(p1, p2 - p1);
				arr->len++;
				//ZEND_HASH_FILL_NEXT();
				p1 = p2 + ZSTR_LEN(delim);
				p2 = php_memnstr(p1, ZSTR_VAL(delim), ZSTR_LEN(delim), endp);
                j++;
			} while (p2 != NULL && --limit > 1);

			if (p1 <= endp) {
				//ZEND_HASH_FILL_GROW();
				//ZEND_HASH_FILL_SET_STR(zend_string_init_fast(p1, endp - p1));
				//ZEND_HASH_FILL_NEXT();
			}
		//} ZEND_HASH_FILL_END();
        return arr;
	}
}

/**
 * Hash table implementation for ArrayObject.
 *
 * Can also use array + linear search? Or binary search (but requires a sorted list).
 *
 * @see https://www.php.net/manual/en/class.arrayobject.php
 * @see https://benhoyt.com/writings/hash-table-in-c/
 * @see https://github.com/benhoyt/ht
 *
 * @todo destructor? Only needed for manually malloc?
 */

/**
 * @see http://www.cse.yorku.ca/~oz/hash.html
 * @see https://stackoverflow.com/questions/7666509/hash-function-for-string
 * @see http://burtleburtle.net/bob/hash/evahash.html
 * @see https://github.com/GNOME/glib/blob/main/glib/ghash.c
 */
unsigned long hash(unsigned char *str)
{
    unsigned long hash = 5381;
    int c;

    while ((c = *str++))
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

    return hash;
}

/**
 * @see https://stackoverflow.com/a/7666799/2138090
 */
uint32_t jenkins_one_at_a_time_hash(uintptr_t* key, size_t len)
{
    uint32_t hash, i;
    for(hash = i = 0; i < len; ++i)
    {
        hash += key[i];
        hash += (hash << 10);
        hash ^= (hash >> 6);
    }
    hash += (hash << 3);
    hash ^= (hash >> 11);
    hash += (hash << 15);
    return hash;
}

struct ArrayObject__entry
{
    // TODO: Hash function assumes this is a string
    char* key;
    uintptr_t* value;
};

typedef struct ArrayObject* ArrayObject;
struct ArrayObject
{
    struct mem mem;
    size_t len;
    size_t size;
    struct ArrayObject__entry* entries;

    // offsetSet
    // offsetGet
    void (*offsetSet) (ArrayObject self, uintptr_t* key, uintptr_t* value);
    uintptr_t* (*offsetGet) (ArrayObject self, uintptr_t* key);
};

void ht_set_entry(ArrayObject self, char* key, void* value);

static bool ht_expand(ArrayObject self)
{
    // Allocate new entries array.
    size_t new_capacity = self->size * 2;
    if (new_capacity < self->size) {
        return false;  // overflow (capacity would be too big)
    }
    struct ArrayObject__entry* new_entries = self->mem.alloc(new_capacity, sizeof(struct ArrayObject__entry));
    if (new_entries == NULL) {
        return false;
    }

    // Iterate entries, move all non-empty ones to new table's entries.
    for (size_t i = 0; i < self->size; i++) {
        struct ArrayObject__entry entry = self->entries[i];
        if (entry.key != NULL) {
            ht_set_entry(new_entries, new_capacity, entry.key, entry.value, NULL);
        }
    }

    // Free old entries array and update this table's details.
    // TODO: Free?
    //free(self->entries);
    self->entries = new_entries;
    self->size = new_capacity;
    return true;
}


/**
 * Internal function to set an entry (without expanding table).
 *
 * @see https://github.com/benhoyt/ht
 */
void ht_set_entry(ArrayObject self, char* key, void* value)
{
    // AND hash with capacity-1 to ensure it's within entries array.
    uint64_t hash_ = hash(key);
    size_t index = (size_t)(hash_ & (uint64_t)(self->size - 1));

    // Loop till we find an empty entry.
    while (self->entries[index].key != NULL) {
        if (strcmp(key, self->entries[index].key) == 0) {
            // Found key (it already exists), update value.
            self->entries[index].value = value;
            //return self->entries[index]key;
        }
        // Key wasn't in this slot, move to next (linear probing).
        index++;
        if (index >= self->size) {
            // At end of entries array, wrap around.
            index = 0;
        }
    }

    // Didn't find key, allocate+copy if needed, then insert it.
    if (self->len != 0) {
        key = strdup(key);
        if (key == NULL) {
            return;
        }
        self->len++;
    }
    self->entries[index].key = (char*)key;
    self->entries[index].value = value;
}
/**
 * @todo Make sure self and value use same allocation strategy.
 */
void ArrayObject__offsetSet(ArrayObject self, uintptr_t* key, uintptr_t* value)
{
    if (value == NULL) {
        return;
    }

    // If length will exceed half of current capacity, expand it.
    if (self->len >= self->size / 2) {
        ht_expand(self);
    }

    // Set entry and update length.
    ht_set_entry(self, key, value);
}

/**
 * Nullable return type - mixed?
 */
uintptr_t* ArrayObject__offsetGet(ArrayObject self, unsigned char* key)
{
    // AND hash with capacity-1 to ensure it's within entries array.
    uint64_t hash_ = hash(key);
    size_t index = (size_t)(hash_ & (uint64_t)(self->size - 1));

    // Loop till we find an empty entry.
    while (self->entries[index].key != NULL) {
        if (strcmp(key, self->entries[index].key) == 0) {
            // Found key, return value.
            return self->entries[index].value;
        }
        // Key wasn't in this slot, move to next (linear probing).
        index++;
        if (index >= self->size) {
            // At end of entries array, wrap around.
            index = 0;
        }
    }
    return NULL;
}

ArrayObject ArrayObject__constructor(ArrayObject self, struct mem m)
{
    self->offsetSet = &ArrayObject__offsetSet;
    self->offsetGet = &ArrayObject__offsetGet;

    self->len  = 0;
    self->size = 100;
    self->entries = m.alloc(m.arena, sizeof(struct ArrayObject__entry) * self->size);
    //self->entries = calloc(self->entries, sizeof(struct ArrayObject__entry) * self->size);

    self->mem = m;

    return self;
}
