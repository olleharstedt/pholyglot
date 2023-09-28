//<?php echo "\x08\x08"; ob_start(); ?>
#include <stdio.h>
#include <stdlib.h>
#include <alloca.h>
#include <stdint.h>
#include <math.h>
#include "phollylib.c"
#include <gc.h>
#define new(x, m) x ## __constructor((x) m.alloc(m.arena, sizeof(struct x)), m)
// alloc new space and copy struct to it
void* clone(void* x, size_t s, struct mem m)
{
    void* new_ = m.alloc(m.arena, s);
    memcpy(new_, x, s);
    return new_;
}
/**
 * Shallow or recursive clone of struct?
 *   keyword clone
 *   __clone magic function
 *
 * Compile with:
 *   cat clone.c | sed -e "s/#__C__//g" | gcc -O1 -I. -Wno-incompatible-pointer-types -xc - -lgc
 *
 * Using "-fsanitize=undefined -fsanitize=address" swallows output when running with valgrind.
 * 21:04 < twkm> combining "sanitizers" with valgrind often produces odd results.
 */
typedef struct Point* Point;
#define class struct
#define public
//<?php
class Point
{
    #define x $x
    public int $x;
    #define y $y
    public int $y;
    #__C__ struct mem mem;
};
//?>
Point Point__constructor(Point self, struct mem m)
{
    self->x = 99;
    self->y = 99;
    self->mem = m;
    return self;
}
//<?php
#if __PHP__//<?php
define("SplDoublyLinkedList", "SplDoublyLinkedList");
define("Point", "Point");
define("malloc", "malloc");
define("arena_mem", "arena_mem");
define("gc_mem", "gc_mem");
define("heap_mem", "heap_mem");
#endif
#define function int
function main()
#undef function
{
    fprintf(STDERR, "start\n");
    #__C__ Point
    $p = new(Point
        #__C__, heap_mem
    );
    $p->x = 10;

    #__C__ Point
    $r = clone($p
        #__C__, sizeof(struct Point), gc_mem
    );
    printf("%ld\n", $p->x);
    printf("%ld\n", $r->x);
    #__C__ free($p);
    fprintf(STDERR, "end\n");
    return 0;
}
//?>
//<?php ob_end_clean(); main();
