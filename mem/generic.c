#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#define EVALUATE(X) _Generic((X), _Bool : "boolean", default : "not boolean")
#define new(x) x ## __constructor(x ## __malloc(sizeof(struct x)))

typedef struct Point* Point;
typedef struct Point* Point__pool;
struct Point {};
struct Point__pool {};
Point Point__constructor(Point $this)
{
    return $this;
}
Point Point__pool__constructor(Point $this) { return $this; }
Point Point__malloc(size_t s) { return malloc(s); }
Point Point__pool__malloc(size_t s) { return malloc(s); }

/**
 * gcc -Wno-incompatible-pointer-types -g generic.c
 */
int main()
{
    // TODO: Pool needs the pool object as argument
    Point p = new(Point__pool);
    printf("%s\n", EVALUATE(p));
    return 0;
}
