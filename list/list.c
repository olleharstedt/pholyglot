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
    struct SplDoublyLinkedList* next_node;
    struct SplDoublyLinkedList* last;
    struct SplDoublyLinkedList* current_node;

    void (*push) (SplDoublyLinkedList self, uintptr_t* item);
    void (*next) (SplDoublyLinkedList self);
    uintptr_t* (*current) (SplDoublyLinkedList self);
    _Bool (*valid) (SplDoublyLinkedList self);
    void (*rewind) (SplDoublyLinkedList self);
};

void SplDoublyLinkedList__push(SplDoublyLinkedList self, uintptr_t* item)
{
    if (self->item == NULL) {
        self->item = item;
    } else {
        SplDoublyLinkedList n = malloc(sizeof(struct SplDoublyLinkedList));
        n->item = item;

        SplDoublyLinkedList current = self;
        while (current->next_node != NULL) {
            current = current->next_node;
        }

        current->next_node = n;
        current->next_node->next_node = NULL;
    }
}

uintptr_t* SplDoublyLinkedList__current(SplDoublyLinkedList self)
{
    return self->current_node->item;
}

SplDoublyLinkedList SplDoublyLinkedList__next(SplDoublyLinkedList self)
{
    if (self->current_node) {
        self->current_node = self->current_node->next_node;
    }
}

_Bool SplDoublyLinkedList__valid(SplDoublyLinkedList self)
{
    return self->current_node != NULL;
}

void SplDoublyLinkedList__rewind(SplDoublyLinkedList self)
{
    self->current_node = self;
}

SplDoublyLinkedList SplDoublyLinkedList__constructor(SplDoublyLinkedList self)
{
    self->push         = &SplDoublyLinkedList__push;
    self->current      = &SplDoublyLinkedList__current;
    self->next         = &SplDoublyLinkedList__next;
    self->valid        = &SplDoublyLinkedList__valid;
    self->rewind       = &SplDoublyLinkedList__rewind;

    self->item         = NULL;
    self->last         = NULL;
    self->next_node    = NULL;
    self->current_node = self;
    return self;
}

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
#endif

/**
 * gcc -g -Wno-incompatible-pointer-types list.c
 * cat list.c | sed -e "s/#__C__//g" | gcc -g -Wno-incompatible-pointer-types -xc -
 */
#define function int
function main()
    #undef function
{
    #__C__ char
    $buffer[100];
    fgets($buffer, 100, stdin);
    #__C__ long
    $i = strtol($buffer, (char **) NULL, 10);

    #__C__ SplDoublyLinkedList
    $list = new(SplDoublyLinkedList);
    #__C__ Point
    $p = new(Point);
    $p->x = 10;
    $p->y = 10;
    $list->push(
        #__C__ $list,
        $p
    );

    for (int $j = 0; $j < $i; $j++) {
        #__C__ Point
        $p2 = new(Point);
        $p2->x = $j;
        $p2->y = 11;
        $list->push(
            #__C__ $list,
            $p2
        );
    }

    $list->rewind(
        #__C__ $list
    );
    do {
        #__C__ Point
        $tmp = $list->current(
            #__C__ $list
        );
        if ($tmp) {
            printf("Current point x = %d\n", $tmp->x);
        }
        $list->next(
            #__C__ $list
        );
    } while ($list->valid(
        #__C__ $list
    ));

    return 0;
}
//?>
//<?php ob_end_clean(); main();
