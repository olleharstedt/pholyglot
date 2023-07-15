//<?php echo "\x08\x08"; ob_start(); ?>
#include <stdio.h>
#include <stdlib.h>
#include <alloca.h>
#include <stdint.h>
#include <math.h>
#include "phollylib.c"
#include <gc.h>
#define new(x, m) x ## __constructor((x) m.alloc(m.arena, sizeof(struct x)), m)
#define intval(x) strtol(x, (char **) NULL, 10);
#define STDIN stdin
typedef struct Point* Point;
#define class struct
#define public
struct mem arena_mem = {0};
struct mem gc_mem = {.alloc = &gc_malloc, .arena = NULL};
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
		$item = $list->current(
			#__C__ $list
		);
        printf("Current point x = %ld\n", $item->x);
		$list->next(
			#__C__ $list
		);
	} while ($list->valid(
		#__C__ $list
	));
}

#define function void
function additems(SplDoublyLinkedList $list, int $nr)
#undef function
{
    #__C__ int
    $j = 0;
    for (; $j < $nr; $j++) {
        #__C__ Point
        // NB: Using mem struct from list here
        // Must be inferred by the compiler somehow.
        $p2 = new(Point
            #__C__ ,$list->mem
        );
        $p2->x = $j;
        $p2->y = 11;
        $list->push(
            #__C__ $list,
            $p2
        );
    }
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
    #__C__ GC_INIT();  // Boehm init
    #__C__ Arena __a = malloc(sizeof(struct Arena));
    #__C__ arena_init(__a, malloc(256), 256);
    #__C__ arena_mem.alloc = &arena_alloc;
    #__C__ arena_mem.arena = __a;
    #__C__ gc_mem.alloc = &gc_malloc;
    #__C__ gc_mem.arena = NULL;

    // TODO: Always require length to fgets to simplify buffer
    // TODO: Always glib string
    //#__C__ char*
    //$buffer = fgets(malloc(sizeof(char) * 10), 10, STDIN);
    #__C__ long
    $i = 10; //intval($buffer);

    #__C__ SplDoublyLinkedList
    $list = new(SplDoublyLinkedList
        #__C__, arena_mem
    );

    additems($list, 10);
    printlist($list);

    #__C__ SplDoublyLinkedList
    $list2 = new(SplDoublyLinkedList
        #__C__, gc_mem
    );

    additems($list2, 10);
    printlist($list2);

    #__C__ arena_free(__a);
    return 0;
}
//?>
//<?php ob_end_clean(); main();
