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

#__C__ typedef struct Body* Body;
class Body {
    #define public float
#define __object_property_x $__object_property_x
    public $__object_property_x;
#undef public
#define public float
#define __object_property_y $__object_property_y
    public $__object_property_y;
#undef public
#define public float
#define __object_property_z $__object_property_z
    public $__object_property_z;
#undef public
#define public float
#define __object_property_vx $__object_property_vx
    public $__object_property_vx;
#undef public
#define public float
#define __object_property_vy $__object_property_vy
    public $__object_property_vy;
#undef public
#define public float
#define __object_property_vz $__object_property_vz
    public $__object_property_vz;
#undef public
#define public float
#define __object_property_mass $__object_property_mass
    public $__object_property_mass;
#undef public

    
// End of C struct def. Class methods are outside the struct.
#__C__ };

#if __PHP__
// End of PHP class def.
};
#endif
#if __PHP__
define("Body", "Body");
#endif
//?>
// Function pointer init
Body Body__constructor(Body $p)
{
    
    return $p;
}
//<?php
#define function int
function main()
{
    #__C__ Body
    $jupiter 
    = new(Body);
    $jupiter->__object_property_x 
    = 10.;
    #__C__ Body
    $bodies 
    #__C__ [1]
    = 
#__C__ {
#if __PHP__
[
#endif
    $jupiter
#if __PHP__
]
#endif
#__C__ }
    ;
    return 0;
}
#undef function
// ?>
// <?php ob_end_clean(); main();
