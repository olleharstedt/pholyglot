//<?php echo "\x08\x08"; ob_start(); ?>
#include <stdio.h>
#include <stdlib.h>
#include <alloca.h>
#include <stdint.h>
#include <math.h>
#include <phollylib.c>
#define new(x) x ## __constructor(malloc(sizeof(struct x)))
#define __new(x, m) x ## __constructor(m(&__a, sizeof(struct x)))
#define intval(x) strtol(x, (char **) NULL, 10);
#define STDIN stdin
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
};
//?>
Point Point__constructor(Point self)
{
    self->x = 99;
    self->y = 99;
    return self;
}

//<?php
#if __PHP__//<?php
define("SplDoublyLinkedList", "SplDoublyLinkedList");
define("Point", "Point");
define("malloc", "malloc");
define("arena_alloc", "arena_alloc");
function __new($c, $f) { return new $c; }
#endif

/**
 * gcc -g -Wno-incompatible-pointer-types list.c
 * cat arena.c | sed -e "s/#__C__//g" | gcc -g -I. -Wno-incompatible-pointer-types -xc -
 * cat arena.c | sed -e "s/#__C__//g" | gcc -xc - -E
 * cat arena.c | sed -e "s/#__C__//g" | gcc -O1 -I. -Wno-incompatible-pointer-types -xc - -fsanitize=undefined -fsanitize=address
 */
#define function int
function main()
#undef function
{
    // TODO: arena
    #__C__ struct Arena __a = {0};
	arena_init(&__a, malloc(255), 256);

    // TODO: Always require length to fgets to simplify buffer
    // TODO: Always glib string
    //#__C__ char*
    //$buffer = fgets(malloc(sizeof(char) * 10), 10, STDIN);
    #__C__ long
    $i = 10; //intval($buffer);

    #__C__ SplDoublyLinkedList
    $list = __new(SplDoublyLinkedList, arena_alloc);

    for (int i = 0; i < 100; i++) {
        #__C__ Point
        $p = __new(Point, arena_alloc);
    }

    arena_free(&__a);

    return 0;
}
