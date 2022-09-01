//<?php echo "\x08\x08"; ob_start(); ?>
#include <stdio.h>
#define class struct
#define __PHP__ 0

//<?php
#define function int
function main() {
#if __PHP__
    ob_start();?>
#endif
    int//<?php ob_end_clean();ob_start();
    $a = 10;
    printf("%d\n", $a);
    return 0;
}
#undef function
//?>
//<?php ob_end_clean(); main();
