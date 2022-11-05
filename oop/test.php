//<?php echo "\x08\x08"; ob_start(); ?>
#include <stdio.h>
#include <glib.h>
#define function 
#define $ 
#define class struct
#define __PHP__ 0
#define new_(x) malloc(sizeof(struct x))
#if __PHP__//<?php
class GString { public $str; public function __construct($str) { $this->str = $str; } }
function g_string_new(string $str) { return new GString($str); }
function g_string_append(GString $s1, string $s2) { return new GString($s1->str . $s2); }
function new_($class) { return new $class; }
#endif//?>
//<?php

  class Point {
      #define public int
  #define __object_property_x $__object_property_x
      public $__object_property_x;
  #undef public
  
      #__C__ int (*getX) ();
  // End of C struct def. Class methods are outside the struct.
  #__C__ };
  
  #if __PHP__
  public function getX(Point $self ): int
  #endif
  #__C__ int Point_getX (struct Point* $self)
  {
      return $self->__object_property_x;
  
  }
  
  #if __PHP__
  // End of PHP class def.\
  };
  #endif
  #if __PHP__
  define("Point", "Point");  // Needed to make new_() work with C macro
  #endif

//?>
struct Point* new_Point(struct Point *$p)
{
    $p->getX = &Point_getX;
    return $p;
}
//<?php
#if __PHP__
function new_Point($p) { return $p; }
#endif
 
#__C__ int
function main()
{
    #__C__ struct Point*
    $p = new_Point(new_(Point));
    $p->__object_property_x 
    = 100;
    printf("%d\n", $p->getX($p));
    return 0;
}
// ?>
// <?php ob_end_clean(); main();
