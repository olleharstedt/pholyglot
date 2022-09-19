//<?php echo "\x08\x08"; ob_start(); ?>
   #include <stdio.h>
   #include <glib.h>
   #define class struct
   #define __PHP__ 0
   #define new_(x) alloca(sizeof(struct x))
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
   
   };
   #if __PHP__
   define("Point", "Point");  // Needed to make new_() work with C macro
   #endif
   #define function int
   function main()
   {
       #__C__ struct Point*
       $b 
       #__C__ [1]
       = 
   #__C__ {
   #if __PHP__
   [
   #endif
       new_(Point)
   #if __PHP__
   ]
   #endif
   #__C__ }
       ;
       printf("%d\n", 0);
       return 0;
   }
   #undef function
   // ?>
   // <?php ob_end_clean(); main();
