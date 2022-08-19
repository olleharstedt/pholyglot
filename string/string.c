//<?php echo "\x08\x08"; ob_start(); ?>
#include <stdio.h>
#include <stdlib.h>
#include <glib.h>
#include "gprintf.h"

#define function 
#define __PHP__ 0

#if __PHP__
//<?php
class GString { public $str; public function __construct($str) { $this->str = $str; } }
function g_string_new($str) { return new GString($str); }
//?>
#endif

//<?php
#__C__ int
function main()
{
    #__C__ GString*
    $a = g_string_new("Hello world!");
    printf("%s\n", $a->str);
    return 0;
}

//?>
//<?php ob_end_clean(); main();
