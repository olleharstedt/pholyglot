//<?php echo "\x08\x08"; ob_start(); ?>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#define array(...) {__VA_ARGS__}
#define array_make(type, i, ...) {.thing = (type[]) array(__VA_ARGS__), .length = i}
#define array_get(type, arr, i) ((type*) arr.thing)[i]
#define new(x) x ## __constructor(malloc(sizeof(struct x)))

typedef struct Body* Body;
struct Body {
    int x;
};
Body Body__constructor(Body this)
{
    this->x = 10;
    return this;
}
typedef struct array array;
struct array {
    size_t length;
    uintptr_t* thing;
};

array array_slice(array old, int offset)
{
    size_t new_length = old.length - offset;
    array new = {
        .length = new_length,
        // TODO: Can't make stack alloc here? Or can, but then it would be copied?
        .thing = malloc(sizeof(uintptr_t) * new_length)
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
int main()
{
    printf("%ld\n", sizeof(double));
    printf("%ld\n", sizeof(int));
    printf("%ld\n", sizeof(uintptr_t));
    printf("%ld\n", sizeof(Body));
    Body body1 = new(Body);
    body1->x = 123;
    array a = array_make(Body, 3, new(Body), new(Body), body1);
    //array a = {.thing = (Body[]) {new(Body), new(Body)}, .length = 2};
    //printf("%p\n", (void*) ((Body*) a.thing)[0]);
    //printf("%p\n", (void*) ((Body*) a.thing)[1]);
    //printf("%p\n", (void*) ((Body*) a.thing)[2]);
    //Body body2 = new(Body);
    /*
    array a = {
        .length = 2,
        .thing = malloc(2 * sizeof(Body))
    };
    */
    array b = array_slice(a, 2);
    printf("slice b[0]->x = %d\n", array_get(Body, b, 0)->x);

    array ints = array_make(long, 5, 11, 22, 33, 44, 55);
    array ints_slice = array_slice(ints, 3);
    printf("ints slice = %ld\n", array_get(long, ints_slice, 0));

    array floats = array_make(double, 3, 1.1, 2.2, 3.3);
    array floats_slice = array_slice(floats, 2);
    printf("floats slice = %f\n", array_get(double, floats_slice, 0));
    return 0;
}
