#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#define EVALUATE(X) _Generic((X), _Bool : "boolean", default : "not boolean")
// #define new(x) x ## __constructor(x ## __malloc(sizeof(struct x)))
#define __new(x) x ## __constructor(malloc(sizeof(struct x)))

typedef struct Point* Point;
struct Point {};
Point Point__constructor(Point $this)
{
    return $this;
}

typedef struct Pool Pool;
struct Pool {
};

enum Mem_enum {
    WASTE = 0,
    REF_COUNT = 1,
    BOEHM = 2,
    STACK = 3,
    POOL = 4
};

typedef union _Mem_type {
    Pool* pool;
    size_t ref_count;
} Mem_type;

typedef struct _Mem {
    int mem_enum;
    Mem_type mem_type;
} Mem;

typedef struct array array;
struct array {
    size_t length;
    uintptr_t* thing;
    Mem mem;
};

/**
 * gcc -Wno-incompatible-pointer-types -g generic.c
 */
int main()
{
    // TODO: Pool needs the pool object as argument
    // Always in scope? Like region? Only one region? Can't pass it around?
    // Can abuse clone?
    // Need wrapping function like __make? Point p = __make(old_point, Point)
    //Point p = /** @mem pool */ new(Point__pool);
    Point p = /** @mem pool */ __new(Point);
    printf("%s\n", EVALUATE(p));
    return 0;
}
