//<?php echo "\x08\x08"; ob_start(); ?>
#include <stdio.h>
#include <stdlib.h>
#include <alloca.h>
#define class struct
#define __PHP__ 0
#define new(x) x ## __constructor(alloca(sizeof(struct x)))
#define array(...) {__VA_ARGS__}
#define array_make(type, i, ...) {.thing = (type[]) array(__VA_ARGS__), .length = i}
#define array_get(type, arr, i) ((type*) arr.thing)[i]
#define count(x) x.length
#define pprintf printf
typedef struct array array;
struct array { void* thing; size_t length; };
//<?php
//?>
typedef struct Body* Body;
//<?php
class Body {
#define public int
#define __object_property_x $__object_property_x
   public $__object_property_x;
#undef public
};
#if __PHP__
define("Body", "Body");
define("int", "int");
function array_get($class, $arr, $i) { return $arr[$i]; }
function array_make($class, $length, ...$values) { return $values; }
function pprintf($format, ...$args) { fwrite( STDOUT, sprintf( $format, ...$args)); }
#endif
//?>
Body Body__constructor(Body $this)
{
    $this->__object_property_x = 10;
    return $this;
}
//<?php

#define function void
function foo(
array
#if __PHP__
&
#endif
$bodies
)
#undef function
{
    //?>
    int
    //<?php
    $i = 0;
    for ($i = 0; $i < count($bodies); $i++) {
        pprintf(
            "%d ",
            array_get(Body, $bodies, $i)->__object_property_x
        );
    }
}

#define function void
function fii(array $numbers)
#undef function
{
    //?>
    int
    //<?php
    $i = 0;
    for ($i = 0; $i < count($numbers); $i++) {
        pprintf(
            "%d ",
            array_get(int, $numbers, $i)
        );
    }
}

#define function int
function main()
#undef function
{
    //?>
    array
    //<?php
    $bodies = array_make(
        Body,
        2,
        new (Body),
        new (Body)
    ) ;
    foo($bodies);
    //?>
    array
    //<?php
    $ints = array_make(int, 3, 1, 2, 3);
    fii($ints);
    return 0;
}
//?>
//<?php main(); ob_end_clean();
