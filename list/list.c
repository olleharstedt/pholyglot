//<?php echo "\x08\x08"; ob_start(); ?>
#include <stdio.h>
#include <stdlib.h>
#include <alloca.h>
#include <stdint.h>
#include <math.h>
#define new(x) x ## __constructor(malloc(sizeof(struct x)))
typedef struct SplDoublyLinkedList* SplDoublyLinkedList;
struct SplDoublyLinkedList {
    uintptr_t* item;
    struct SplDoublyLinkedList* next;
    struct SplDoublyLinkedList* last;
    void (*push) (SplDoublyLinkedList self, uintptr_t* item);
};

void SplDoublyLinkedList__push(SplDoublyLinkedList self, uintptr_t* item)
{
    SplDoublyLinkedList n = malloc(sizeof(struct SplDoublyLinkedList));
    n->item = item;
    n->push = &SplDoublyLinkedList__push;

    SplDoublyLinkedList current = self;
    while (current->next != NULL) {
        current = current->next;
    }

    current->next = n;
    current->next->next = NULL;
}

SplDoublyLinkedList SplDoublyLinkedList__constructor(SplDoublyLinkedList self)
{
    self->push = &SplDoublyLinkedList__push;
    self->item = NULL;
    self->last = NULL;
    self->next = NULL;
    return self;
}

typedef struct Point* Point;
#define class struct
#define public
//<?php
class Point
{
    public int $x;
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
#endif

/**
 * gcc -g -Wno-incompatible-pointer-types list.c
 */
#define function int
function main()
#undef function
{
    #__C__ SplDoublyLinkedList
    $list = new(SplDoublyLinkedList);
    #__C__ Point
    $p = new(Point);
    $p->x = 10;
    $p->y = 10;
    $list->push(
        #__C__ list,
        $p
    );

    #__C__ Point
    $p2 = new(Point);
    $list->push(
        #__C__ list,
        $p2
    );

    #__C__ SplDoublyLinkedList
    $current = $list;
    do {
        if ($current->item) {
            $tmp = $current->item;
            printf("Current point x = %d\n", $tmp->x);
        }
        printf("Hey\n");
        $current = $current->next;
    } while (current);

    return 0;
}
//?>
//<?php ob_end_clean(); main();
