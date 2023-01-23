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
struct Point
{
    int x;
    int y;
};
Point Point__constructor(Point self)
{
    self->x = 99;
    self->y = 99;
    return self;
}

/**
 * gcc -g -Wno-incompatible-pointer-types list.c
 */
int main()
{
    SplDoublyLinkedList l = new(SplDoublyLinkedList);
    Point p = new(Point);
    p->x = 10;
    p->y = 10;
    l->push(l, p);

    Point p2 = new(Point);
    l->push(l, p2);

    SplDoublyLinkedList current = l;
    do {
        if (current->item) {
            printf("Current point x = %d\n", ((Point) current->item)->x);
        }
        printf("Hey\n");
        current = current->next;
    } while (current);

    return 0;
}
