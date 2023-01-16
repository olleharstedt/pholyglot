//<?php echo "\x08\x08"; ob_start(); ?>
#include <stdio.h>
#include <stdlib.h>
#include <alloca.h>
#include <stdint.h>
#include <math.h>
#define class struct
#define __PHP__ 0
#define new(x) x ## __constructor(alloca(sizeof(struct x)))
#define array(...) {__VA_ARGS__}
#define array_make(type, i, ...) {.thing = (type[]) array(__VA_ARGS__), .length = i}
#define array_get(type, arr, i) ((type*) arr.thing)[i]
#define count(x) x.length
#define pprintf printf
typedef struct array array;
struct array {uintptr_t* thing; size_t length; };
array array_slice(array old, int offset)
{
    size_t new_length = old.length - offset;
    array new = {
        .length = new_length,
        .thing = malloc(sizeof(uintptr_t) * new_length)
    };
    size_t j = 0;
    for (size_t i = offset; i < old.length; i++) {
        new.thing[j] = old.thing[i];
        j++;
    }
    return new;
}
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

//?>
void foo(array $bodies)
//<?php
#if __PHP__
function foo(array &$bodies)
#endif
{
    //?>
    //Body* $bodies = $__bodies.thing;
    //<?php
    //?>
    int
    //<?php
    $i = 0;
    for ($i = 0; $i < count($bodies); $i++) {
        pprintf(
            "%d ",
            array_get(Body, $bodies, $i)->__object_property_x
            //$bodies[$i]->__object_property_x
        );
    }
    array_get(Body, $bodies, 0)->__object_property_x = 99;
}

#define function void
function fii(array $numbers)
#undef function
{

   int
   //<?php
   $i 
   = 0;
   
       for (; $i < count($numbers); $i = $i + 1) {
           //?>
   int
   //<?php
   $val 
   = array_get(int, $numbers, $i);
    printf("%d ", $val);
   
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
    printf("%d ", array_get(Body, $bodies, 0)->__object_property_x);
    //?>
    array
    //<?php
    $ints = array_make(int, 4, 1, 2, 3, 5);
    fii($ints);
    fprintf(stderr, "hello");

    //?>
    array
        //<?php
        $arr 
        = array_make(long, 3, 1, 2, 3);
    //?>
    array
        //<?php
        $arr2 
        = array_slice($arr, 1);
    //?>
    long
        //<?php
        $i 
        = array_get(long, $arr2, 0);
        printf("\nshould be 2: %ld\n", $i);

    double d = sqrt(1.123);
    d = d + 10.0;
    printf("%f\n", d);
    return 0;
}
//?>
//<?php main(); ob_end_clean();
