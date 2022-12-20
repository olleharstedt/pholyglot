//<?php echo "\x08\x08"; ob_start(); ?>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#define array(...) {(uintptr_t) __VA_ARGS__}
#define array_make(i, ...) {.thing = (uintptr_t[]) array(__VA_ARGS__), .length = i}
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
    // TODO: Must have correct length
    array new = array_make(old.length - offset, 0, 0, 0, 0);
    int j = 0;
    for (int i = offset; i < old.length; i++) {
        new.thing[j] = (uintptr_t) old.thing[i];
        j++;
    }
    return new;
}

/**
 * gcc -Wall -Werror -pedantic-errors -g slice.c
 * gcc -Wall -Werror -pedantic-errors -Wno-int-conversion -g slice.c
 */
int main()
{
    array a = array_make(2, new(Body), new(Body));
    //array a = {.thing = (Body[]) {new(Body), new(Body)}, .length = 2};
    //printf("%p\n", (void*) ((Body*) a.thing)[0]);
    //printf("%p\n", (void*) ((Body*) a.thing)[1]);
    //printf("%p\n", (void*) ((Body*) a.thing)[2]);
    //Body body1 = new(Body);
    //Body body2 = new(Body);
    /*
    array a = {
        .length = 2,
        .thing = malloc(2 * sizeof(Body))
    };
    */
    array b = array_slice(a, 1);
    printf("slice b[0]->x = %d\n", array_get(Body, b, 0)->x);

    array ints = array_make(3, 11, 22, 33);
    array ints_slice = array_slice(ints, 1);
    // TODO: float doesn't work
    printf("ints slice = %d\n", array_get(int, ints_slice, 0));
    return 0;
}
