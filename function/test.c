//<?php echo "\x08\x08"; ob_start(); ?>
   #include <stdio.h>
   #include <glib.h>
   #define $ 
   #define class struct
   #define __PHP__ 0
   #define new_(x) alloca(sizeof(struct x))
   #if __PHP__//<?php
   class GString { public $str; public function __construct($str) { $this->str = $str; } }
   function g_string_new(string $str) { return new GString($str); }
   function g_string_append(GString $s1, string $s2) { return new GString($s1->str . $s2); }
   function new_($class) { return new $class; }
   function ignore($a) { return null; }
   #endif//?>
   //<?php
   #define function int
   function foo(int $c)
   {
       return $c + 20;
   }
   #undef function
   #define function int
   function main()
   {
       #__C__ int
       $b 
       = foo(10 + 20);
       return $b + 30;
   }
   #undef function
   // ?>
   //<?php ob_end_clean(); main();
