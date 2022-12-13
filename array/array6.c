//<?php echo "\x08\x08"; ob_start(); ?>
#include <stdio.h>
#include <stdlib.h>
#define BUILD_BUG_ON_ZERO(e) (0)
#define __same_type(a, b) __builtin_types_compatible_p(typeof(a), typeof(b))
#define __must_be_array(a) BUILD_BUG_ON_ZERO(__same_type((a), &(a)[0]))
#define ARRAYSIZE(arr) (sizeof(arr) / sizeof((arr)[0]) + __must_be_array(arr))
#define array(...) {__VA_ARGS__}
#define pprintf printf
//<?php
#if __PHP__
function pprintf($format, ...$args) { fwrite( STDOUT, sprintf( $format, ...$args)); }
#endif
#define function int
function main()
#undef function
{
    //?>
    int
    //<?php
    ints
    [5] = array(1, 2, 3, 4, 5);
    int i = 0;
    for (i = 0; i < ARRAYSIZE(ints); ++i) {
        pprintf("%d ", ints[i]);
    }
}
//<?php main(); ob_end_clean();
