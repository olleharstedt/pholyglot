//<?php echo "\x08\x08"; ob_start(); ?>
#include <stdio.h>
#include <stdlib.h>

struct Point {
    int $x;
    char* $class_name;
};

// Dynamic structure - region or never free
struct List {
    int $x;
    struct List* next;
};

struct Point foo()
{
    struct Point* p = alloca(sizeof(struct Point));
    p->$class_name = "Point";
    p->$x = 10;
    return *p;
}

struct Point* bar()
{
    struct Point* p = alloca(sizeof(struct Point));
    p->$class_name = "Point";
    p->$x = 10;
    return p;
}

int main()
{
    //21:42 < fizzie> struct Point *p = (struct Point[]){foo()};  // just to be silly
    struct Point* p = bar();
    printf("%d\n", p->$x);
    printf("%s\n", p->$class_name);
    return 0;
}
