//<?php echo "\x08\x08"; ob_start(); ?>
#include <stdio.h>
#include <glib.h>
#define function 
#define $ 
#define class struct
#define __PHP__ 0
#if __PHP__//<?php
class GString { public $str; public function __construct($str) { $this->str = $str; } }
function g_string_new(string $str) { return new GString($str); }
function g_string_append(GString $s1, string $s2) { return new GString($s1->str . $s2); }
#endif//?>
//<?php

class Point {
    #define public int
#define __object_property_x $__object_property_x
    public $__object_property_x;
#undef public
#define public int
#define __object_property_y $__object_property_y
    public $__object_property_y;
#undef public

};

function new_($class)
{
    return new $class;
}

#__C__ int
function main()
{
    #__C__ struct Point*
    $p 
    = new_("Point");
    #__C__ int
    $p->__object_property_x 
    = 1;
     printf("%d", $p->__object_property_x);
    return 0;
}
//?>
//<?php ob_end_clean(); main();
