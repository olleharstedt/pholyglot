//<?php echo "\x08\x08"; ob_start(); ?>
#include <stdio.h>
#include <glib.h>
#define $ 
#define class struct
#define __PHP__ 0
#define new(x) x ## __constructor(alloca(sizeof(struct x)))
#if __PHP__//<?php
class GString { public $str; public function __construct($str) { $this->str = $str; } }
function g_string_new(string $str) { return new GString($str); }
function g_string_append(GString $s1, string $s2) { return new GString($s1->str . $s2); }
#endif//?>
//<?php

#__C__ typedef struct Point* Point;
class Point {
    #define public int
#define __object_property_x $__object_property_x
    public $__object_property_x;
#undef public

    #__C__ int (*getX) (Point $self);
// End of C struct def. Class methods are outside the struct.
#__C__ };

#if __PHP__
public function getX(Point $self): int
#endif
#__C__ int Point__getX (Point $self)
{
     printf("Mooo");
    return $self->__object_property_x;

}

#if __PHP__
// End of PHP class def.
};
#endif
#if __PHP__
define("Point", "Point");
#endif
//?>
// Function pointer init
Point Point__constructor(Point $p)
{
    $p->getX = &Point__getX;

    return $p;
}
//<?php
#define function int
function main()
{
     printf("Hello, world!\n");
    #__C__ Point
    $p 
    = new(Point);
    $p->__object_property_x 
    = 10;
     printf("%d", $p->getX($p));
    return 0;
}
#undef function
// ?>
// <?php ob_end_clean(); main();
