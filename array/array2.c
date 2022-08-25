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
#__C__ int
	$arr
#__C__ [3]
        = 
#__C__ {
#if __PHP__
	[
#endif
	1, 2, 3
#if __PHP__
	]
#endif
#__C__ }
	;
	printf("%d", $arr[0]);
	return 0;
}
// ?>
// <?php ob_end_clean(); main();
