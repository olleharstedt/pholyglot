//<?php echo "\x08\x08"; ob_start(); ?>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#define array(...) {__VA_ARGS__}
#define array_make(type, i, ...) {.thing = (type[]) array(__VA_ARGS__), .length = i}
#define array_get(type, arr, i) ((type*) arr.thing)[i]
#define count(x) x.length
#define MALLOC(x, y) _Generic((x), int: malloc(y))
#define EVALUATE(X) _Generic((X), _Bool : "boolean", default : "not boolean")
// #define new(x) _Generic((x), int: do())
#define new(X) _Generic((X), _Bool : "boolean", default : "not boolean")
#define malloc_size(x) alloca(sizeof(uintptr_t) * x)
#define class struct
#define _printf printf

typedef struct Body* Body;
//<?php
class Body {
#define public int
#define Body__x $Body__x
    public $Body__x;
#undef public
};
#if __PHP__
define("Body", "Body");
define("long", "int");
define("double", "float");
function array_make($class, $length, ...$values) { return $values; }
function array_get($class, $arr, $i) { return $arr[$i]; }
function malloc_size() {return null;}
function _printf($format, ...$args) { fwrite( STDOUT, sprintf( $format, ...$args)); }
#endif
//?>
Body Body__constructor(Body $this)
{
    $this->Body__x = 10;
    return $this;
}
typedef struct array array;
struct array {
    size_t length;
    uintptr_t* thing;
    // TODO: ref count?
    // TODO: pool pointer?
    // TODO: alloc strategy enum? union? or function pointer to malloc?
};
typedef struct Pool Pool;
struct Pool {
};
struct array__pool {
    size_t length;
    uintptr_t* thing;
    Pool* pool;
};
struct array__boehm {
    size_t length;
    uintptr_t* thing;
};
struct array__refcount {
    size_t length;
    uintptr_t* thing;
    size_t count;
};
array array_slice(array old, int offset)
{
    size_t new_length = old.length - offset;
    array new = {
        .length = new_length,
        // TODO: How to allow both alloca and malloc/Boehm here?
        // Inline only way? But can't inline very big functions.
        // How to know if a function will allocate? Only certain expressions will allocate
        // TODO: Use same alloc strat as "old" variable.
        // TODO: Never use stack alloc but rather pools with similar semantics (free at end of function).
        // But "free at end of function" doesn't allow value types?
        // TODO: With pool you also need to pass around the pool. Can be part of variable header?
        // Start each function with a "stack pool" used by stack alloc?
        // TODO: malloc comes from function pointer in "old" array struct?
        .thing = MALLOC(old, sizeof(uintptr_t) * new_length)
        //.thing = alloca(sizeof(uintptr_t) * new_length)
        // .thing = old.malloc(sizeof(uintptr_t) * new_length)
    };
    size_t j = 0;
    for (size_t i = offset; i < old.length; i++) {
        new.thing[j] = old.thing[i];
        j++;
    }
    return new;
}
/**
 * gcc -Wall -Werror -pedantic-errors -g poly.c
 * gcc -Wall -Werror -pedantic-errors -Wno-int-conversion -g poly.c
 * gcc -Wno-incompatible-pointer-types -g poly.c
 *
 * To see macro expansions:
 *   gcc -E poly.c
 */
//<?php
#define function int
function main()
#undef function
{
    //?>
    Body //<?php
    $body1 = MALLOC(10, 10);
    return 0;
}
//?>
//<?php main(); ob_end_clean();
