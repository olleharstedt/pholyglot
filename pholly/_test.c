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

typedef struct Body* Body;
class Body {
    #define public int
#define __object_property_a $__object_property_a
    public $__object_property_a;
#undef public

    
// End of C struct def. Class methods are outside the struct.
};

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
    return 0;
}
#undef function
// ?>
// <?php ob_end_clean(); main();
