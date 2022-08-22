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
    $arr = [];
    $arr = [1, "Moo"];
    $arr = [1, 2, 3];
    $arr[] = 4;
    $arr["moo"] = 4;
    $arr = [
        "moo" => 4,
        "foo" => 5
    ];
    // return $arr;
    return 0;
}
//?>
//<?php ob_end_clean(); main();
