//<?php echo "\x08\x08"; ob_start(); ?>
#include <stdio.h>
#include <glib.h>
#define function 
#define __PHP__ 0
#if __PHP__//<?php
class GString { public $str; public function __construct($str) { $this->str = $str; } }
function g_string_new(string $str) { return new GString($str); }
function g_string_append(GString $s1, string $s2) { return new GString($s1->str . $s2); }
#endif//?>
//<?php
#__C__ int
function main()
{
    #__C__ GString*
    $b = g_string_append(g_string_new("Hello "), g_string_new("world!")->str);
    printf("%s\n", $b->str);
    return 0;
}
//?>
//<?php ob_end_clean(); main();
