//<?php echo "\x08\x08"; ob_start(); ?>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#define array(...) {__VA_ARGS__}
#define array_make(type, i, ...) {.thing = (type[]) array(__VA_ARGS__), .length = i}
#define array_get(type, arr, i) ((type*) arr.thing)[i]
#define count(x) x.length
#define new(x) x ## __constructor(alloca(sizeof(struct x)))
#define malloc_size(x) alloca(sizeof(uintptr_t) * x)
#define class struct
#define _printf printf
#define FORCE_INLINE __attribute__((always_inline)) inline

#define AS ,
#define FIRST_ARG_(N, ...) N
#define FIRST_ARG(args) FIRST_ARG_ args
#define foreach(...) for(int i = 0, $f; i < count(FIRST_ARG((__VA_ARGS__))); i++, $f = array_get(double, $floats, i));

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
    // TODO: alloc strategy enum? union?
};
FORCE_INLINE array array_slice(array old, int offset)
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
        .thing = alloca(sizeof(uintptr_t) * new_length)
    };
    size_t j = 0;
    for (size_t i = offset; i < old.length; i++) {
        new.thing[j] = old.thing[i];
        j++;
    }
    return new;
}
/**
 * gcc -Wall -Werror -pedantic-errors -g slice.c
 * gcc -Wall -Werror -pedantic-errors -Wno-int-conversion -g slice.c
 * gcc -Wno-incompatible-pointer-types -g slice.c
 */
//<?php
#define function int
function main()
#undef function
{
    /*
    printf("%ld\n", sizeof(double));
    printf("%ld\n", sizeof(int));
    printf("%ld\n", sizeof(uintptr_t));
    printf("%ld\n", sizeof(Body));
    printf("%ld\n", sizeof(size_t));
    */
    //?>
    Body //<?php
    $body1 = new(Body);
    $body1->Body__x = 123;
    //?>
    array //<?php
    $a = array_make(Body, 3, new(Body), new(Body), $body1);
    //?>
    array //<?php
    $b = array_slice($a, 2);
    _printf("slice b[0]->x = %d\n", array_get(Body, $b, 0)->Body__x);
    //?>
    array//<?php
    $ints = array_make(long, 5, 11, 22, 33, 44, 55);
    //?>
    array//<?php
    $ints_slice = array_slice($ints, 3);
    _printf("ints slice = %ld\n", array_get(long, $ints_slice, 1));
    //?>
    array//<?php
    $floats = array_make(double, 3, 1.1, 2.2, 3.3);
    //?>
    array//<?php
    $floats_slice = array_slice($floats, 2);
    _printf("floats slice = %f\n", array_get(double, $floats_slice, 0));

    //foreach ($floats AS $f) {
        //?>
        //_printf("f = %f\n", $f);
    //}

    return 0;
}
//?>
//<?php main(); ob_end_clean();
