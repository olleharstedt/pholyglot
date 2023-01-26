//<?php echo "\x08\x08"; ob_start(); ?>
#include <stdio.h>
#include <stdlib.h>
#include <alloca.h>
#include <stdint.h>
#include <math.h>
#include "phollylib.c"
#include <gc.h>
#define __new(x, m) x ## __constructor((x) m(__a, sizeof(struct x)))
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
define("gc_malloc", "gc_malloc");
function __new($c, $f) { return new $c; }
#endif


#define function void
function printlist(SplDoublyLinkedList $list)
#undef function
{
    $list->rewind(
        #__C__ $list
    );
    do {
        #__C__ Point
        $tmp = $list->current(
            #__C__ $list
        );
        if ($tmp) {
            printf("Current point x = %ld\n", $tmp->x);
        } else {
            printf("No current :(\n");
        }   
        $list->next(
            #__C__ $list
        );
    } while ($list->valid(
        #__C__ $list
    ));

}

/**
 * gcc -g -Wno-incompatible-pointer-types list.c
 * cat list.c | sed -e "s/#__C__//g" | gcc -g -I. -Wno-incompatible-pointer-types -xc - -lgc
 * cat list.c | sed -e "s/#__C__//g" | gcc -xc - -E
 * cat list.c | sed -e "s/#__C__//g" | gcc -O1 -I. -Wno-incompatible-pointer-types -xc - -fsanitize=undefined -fsanitize=address -lgc
 */
#define function int
function main()
#undef function
{
    #__C__ GC_INIT();
    #__C__ Arena __a = malloc(sizeof(struct Arena));
    #__C__ arena_init(__a, malloc(256), 256);

    // TODO: Always require length to fgets to simplify buffer
    // TODO: Always glib string
    //#__C__ char*
    //$buffer = fgets(malloc(sizeof(char) * 10), 10, STDIN);
    #__C__ long
    $i = 10; //intval($buffer);

    #__C__ SplDoublyLinkedList
    $list = __new(SplDoublyLinkedList, arena_alloc);
    #__C__ $list->alloc = &arena_alloc;
    #__C__ $list->mem   = __a;

    #__C__ int
    $j = 0;
    for (; $j < $i; $j++) {
        #__C__ Point
        $p2 = __new(Point, arena_alloc);
        $p2->x = $j;
        $p2->y = 11;
        $list->push(
            #__C__ $list,
            $p2
        );
    }

    printlist($list);

    #__C__ SplDoublyLinkedList
    $list2 = __new(SplDoublyLinkedList, gc_malloc);
    #__C__ $list2->alloc = &gc_malloc;
    #__C__ $list2->mem   = NULL;

    #__C__ int
    $k = 0;
    for (; $k < $i; $k++) {
        #__C__ Point
        $p2 = __new(Point, gc_malloc);
        if (!$p2) {
            printf("No point\n");
        }
        $p2->x = $k * 22;
        $p2->y = 11;
        $list2->push(
            #__C__ $list2,
            $p2
        );
    }

    printlist($list2);

    #__C__ arena_free(__a);
    return 0;
}
//?>
//<?php ob_end_clean(); main();
