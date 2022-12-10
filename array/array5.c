//<?php echo "\x08\x08"; ob_start(); ?>
#include <stdio.h>
#include <stdlib.h>
#include <alloca.h>
#define class struct
#define __PHP__ 0
#define new(x) x ## __constructor(alloca(sizeof(struct x)))
#define array(...) {__VA_ARGS__}
#define array_get(type, arr, i) ((type*) arr.thing)[i]
#define count(x) x.length
typedef struct array array;
struct array
{
    void* thing;
    size_t length;
};
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
function array_get($class, $arr, $i)
{
    return $arr[$i];
}
#endif
//?>
Body Body__constructor(Body $this)
{
    $this->__object_property_x = 10;
    return $this;
}
//<?php


#define function void
function foo(array $numbers)
#undef function
{
    #__C__ int
    $i = 0;
    for ($i = 0; $i < count($numbers); $i++) {
        printf(
            "%d \n",
            array_get(Body, $numbers, $i)->__object_property_x
        );
    }
}

#define function int
function main()
#undef function
{
    /*
    #__C__ int 
    $_nrs
    #__C__ [5]
    = array(1, 2, 3, 4);
    array $nrs = {&$_nrs, 4};
     */

    #__C__ array
    $test =
        #__C__ {.thing = (Body[])
        array(new(Body), new(Body))
        #__C__ , .length = 2}
    ;
    foo($test);

    return 0;
}
//?>
//<?php ob_end_clean(); main();
