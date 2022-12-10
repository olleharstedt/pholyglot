//<?php echo "\x08\x08"; ob_start(); ?>
#include <stdio.h>
#include <stdlib.h>
#define array(...) {__VA_ARGS__}
#define count(x) x.length
typedef struct array array;
struct array
{
    void* thing;
    size_t length;
};
//<?php

#define function void
function foo(array $numbers)
{
    #__C__ int
    $i = 0;
    for ($i = 0; $i < count($numbers); $i++) {
        printf(
            "%d \n",
            (
                #__C__ (int*)
                $numbers
                #__C__ .thing
            )[$i]
        );
    }
}
#undef function

#define function int
function main()
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
        #__C__ {.thing = (int[])
        array(1, 2, 3)
        #__C__ , .length = 3}
    ;
    foo($test);

    return 0;
}
#undef function
//?>
//<?php ob_end_clean(); main();
