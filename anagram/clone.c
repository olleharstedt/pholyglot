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
#define clone(x, m) memcpy(x)
//<?php
/**
 * Shallow or recursive clone of struct?
 *   keyword clone
 *   __clone magic function
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
#define function int
function main()
#undef function
{
    #__C__ Point
    $p = new(Point
    #__C__, heap_mem
    );
    $p->x = 10;

    #__C__ Point
    $r = clone($p
    #__C__, heap_mem
    );
    printf("%d\n", $r->x);
}
//?>
//<?php ob_end_clean(); main();
