//<?php echo "\x08\x08"; ob_start(); ?>
#include <stdio.h>
#include <glib.h>
#include <stdint.h>
#define class struct
#define __PHP__ 0
#define new(x) x ## __constructor(alloca(sizeof(struct x)))
#define array(...) {__VA_ARGS__}
#define array_make(type, i, ...) {.thing = (type[]) array(__VA_ARGS__), .length = i}
#define array_get(type, arr, i) ((type*) arr.thing)[i]
#define count(x) x.length
#define pprintf printf
typedef struct array array;
struct array { uintptr_t* thing; size_t length; };
array array_slice(array old, int offset)
{
    size_t new_length = old.length - offset;
    if (new_length < 1) {
        return (array) {.length = 0, .thing = NULL};
    }
    pprintf("%ld\n", new_length);
    array new = {
        .length = new_length,
        .thing = malloc(sizeof(uintptr_t) * new_length)
    };
    size_t j = 0;
    for (size_t i = offset; i < old.length; i++) {
        new.thing[j] = old.thing[i]; j++;
    }
    return new;
}
#if __PHP__//<?php
class GString { public $str; public function __construct($str) { $this->str = $str; } }
function g_string_new(string $str) { return new GString($str); }
function g_string_append(GString $s1, string $s2) { return new GString($s1->str . $s2); }
define("int", "int");
define("float", "float");
define("string", "string");
function array_get($type, $arr, $i) { return $arr[$i]; }
function array_make($type, $length, ...$values) { return $values; }
function pprintf($format, ...$args) { fwrite( STDOUT, sprintf( $format, ...$args)); }
#endif//?>
//<?php

//?>
typedef struct Body* Body;
//<?php
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
//?>
};
//<?php

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
//?>
void advance(array $bodies, float $dt)
//<?php
#if __PHP__
function advance(array &$bodies, float $dt): void
#endif
{
    }
//?>
float energy(array $bodies)
//<?php
#if __PHP__
function energy(array &$bodies): float
#endif
{
    }
#define function int
function main()
#undef function
{
    //?>
    double
    //<?php
    $daysperyear 
    = 365.24;
    //?>
    double
    //<?php
    $pi 
    = 3.1415926535897931;
    //?>
    double
    //<?php
    $solarmass 
    = 4. * $pi * $pi;
    //?>
    Body
    //<?php
    $jupiter 
    = new(Body);
    $jupiter->__object_property_x 
    = 4.8414314424647209;
    $jupiter->__object_property_y 
    = -1.1603200440274284;
    $jupiter->__object_property_z 
    = -0.10362204447112311;
    $jupiter->__object_property_vx 
    = 0.0016600766427440369 * $daysperyear;
    $jupiter->__object_property_vy 
    = 0.0076990111841974043 * $daysperyear;
    $jupiter->__object_property_vz 
    = -6.90460016972063e-05 * $daysperyear;
    $jupiter->__object_property_mass 
    = 0.00095479193842432661 * $solarmass;
    //?>
    Body
    //<?php
    $saturn 
    = new(Body);
    $saturn->__object_property_x 
    = 8.34336671824458;
    $saturn->__object_property_y 
    = 4.1247985641243048;
    $saturn->__object_property_z 
    = -0.40352341711432138;
    $saturn->__object_property_vx 
    = -0.0027674251072686241 * $daysperyear;
    $saturn->__object_property_vy 
    = 0.0049985280123491724 * $daysperyear;
    $saturn->__object_property_vz 
    = 2.3041729757376393e-05 * $daysperyear;
    $saturn->__object_property_mass 
    = 0.00028588598066613081 * $solarmass;
    //?>
    Body
    //<?php
    $uranus 
    = new(Body);
    $uranus->__object_property_x 
    = 12.894369562139131;
    $uranus->__object_property_y 
    = -15.111151401698631;
    $uranus->__object_property_z 
    = -0.22330757889265573;
    $uranus->__object_property_vx 
    = 0.0029646013756476162 * $daysperyear;
    $uranus->__object_property_vy 
    = 0.0023784717395948095 * $daysperyear;
    $uranus->__object_property_vz 
    = -2.9658956854023756e-05 * $daysperyear;
    $uranus->__object_property_mass 
    = 4.366244043351563e-05 * $solarmass;
    //?>
    Body
    //<?php
    $neptune 
    = new(Body);
    $neptune->__object_property_x 
    = 15.379697114850917;
    $neptune->__object_property_y 
    = -25.919314609987964;
    $neptune->__object_property_z 
    = 0.17925877295037118;
    $neptune->__object_property_vx 
    = 0.0026806777249038932 * $daysperyear;
    $neptune->__object_property_vy 
    = 0.001628241700382423 * $daysperyear;
    $neptune->__object_property_vz 
    = -9.5159225451971587e-05 * $daysperyear;
    $neptune->__object_property_mass 
    = 5.1513890204661145e-05 * $solarmass;
    //?>
    Body
    //<?php
    $sun 
    = new(Body);
    $sun->__object_property_x 
    = 0.;
    $sun->__object_property_y 
    = 0.;
    $sun->__object_property_z 
    = 0.;
    $sun->__object_property_vx 
    = 0.;
    $sun->__object_property_vy 
    = 0.;
    $sun->__object_property_vz 
    = 0.;
    $sun->__object_property_mass 
    = $solarmass;
    //?>
    array
    //<?php
    $bodies 
    = array_make(Body, 5, $sun, $jupiter, $saturn, $uranus, $neptune);
    //?>
    long
    //<?php
    $i 
    = 0;
    //?>
    long
    //<?php
    $__i 
    = 0;
    
        for (; $__i < count($bodies); $__i = $__i + 1) {
            //?>
    Body
    //<?php
    $body 
    = array_get(Body, $bodies, $__i);
     printf("%f\n", $body->__object_property_x);
    $i++;
//?>
    array
    //<?php
    $arr2 
    = array_slice($bodies, $i);
     if (count($arr2) > 0) printf("%f", array_get(Body, $arr2, 0)->__object_property_x);
    
        }
     printf("%f", array_get(Body, $bodies, 0)->__object_property_x);
    return 0;
}
// ?>
// <?php ob_end_clean(); main();
