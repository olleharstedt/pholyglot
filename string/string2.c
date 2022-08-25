//<?php echo "\x08\x08"; ob_start(); ?>
#include <stdio.h>
#include <glib.h>
#define function 
//<?php
#__C__ int
function main()
{
    #__C__ GString*
    $str = g_string_append(g_string_append(g_string_new("Hello"), g_string_new(" world")->str), g_string_new("!")->str);
    printf($str->str);
    return 0;
}
// ?>
// <?php ob_end_clean(); main();
