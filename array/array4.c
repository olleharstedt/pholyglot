//<?php echo "\x08\x08"; ob_start(); ?>
#include <stdio.h>
#include <stdlib.h>
#define class struct
#define __PHP__ 0
#define new(x) x ## __constructor(alloca(sizeof(struct x)))
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
#endif

//?>
Body Body__constructor(Body $body)
{
    return $body;
}
//<?php

#define function void
function advance(
#define array Body*
    array $bodies,
#undef array
    float $dt
)
{
    #__C__ int
    $i = 0;
    for (; $i < 3; $i++) {
        printf("%d\n", $bodies[$i]->__object_property_x);
    }
}
#undef function
#undef array

#define function int
function main()
{
    #__C__ Body
    $b = new(Body);
    $b->__object_property_x = 10;
    //?>
    #__C__ Body
    $bodies
    #__C__ [3]
    =
   #__C__ {
   #if __PHP__
   [
   #endif
        $b,
        $b,
        $b
   #if __PHP__
   ]
   #endif
   #__C__ }
    ;
    advance($bodies, 1.10);
    return 0;
}
// ?>
// <?php ob_end_clean(); main();
