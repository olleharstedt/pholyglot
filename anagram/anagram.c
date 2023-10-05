//<?php echo "\x08\x08"; ob_start(); ?>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "phollylib.c"

/**
21:59 < DPA> olle: Oh, yes, nested generics are a pain. There are a few tricks you may want to know about.
21:59 < DPA> First of, the semantics of non-matching branches is checked, too, a macro to force the expected type is handy. Someone here came up with this: #define G_FORCE(T, V)
             _Generic((V), T: (V), default: (T){0}) \n _Generic((X), A: G_FORCE(A, (X)).x, B: G_FORCE(B, (X)).y)
22:00 < DPA> Secondly, the nested generics all need to match, so consider putting default branches in them. You can use a static_assert and a generic in it resolving to 0 or 1 to make it compile only in the intended cases.
 */

//<?php
#if __PHP__//<?php
function COMPARE_MIXED($a, $b)
{
    return $a == $b;
}
function GET_MIXED_STRING($a)
{
    return $a;
}
function GET_STRING($a)
{
    return $a;
}
function ph_smartstr_new($a, $_)
{
    return $a;
}
#endif

/**
 * Compile with:
 *   gcc -g -I. -Wno-incompatible-pointer-types -xc -fsanitize=undefined -fsanitize=address -lgc anagram.c
 *   gcc -g -I. -Wno-incompatible-pointer-types -xc anagram.c -lgc
 *   gcc -xc -lgc -I. -I/usr/include/glib-2.0 -I/usr/lib/x86_64-linux-gnu/glib-2.0/include/ anagram.c
 *   cat anagram.c | sed -e "s/#__C__//g" | gcc -g -I. -Wno-incompatible-pointer-types -xc -fsanitize=undefined -fsanitize=address -xc - -lgc
 */
#define function int
function main()
#undef function
{
    #__C__ GC_INIT();  // Boehm init
    #__C__ Arena __a = malloc(sizeof(struct Arena));
    #__C__ arena_init(__a, malloc(256), 256);
    #__C__ arena_mem.alloc = &arena_alloc;
    #__C__ arena_mem.arena = __a;

    // @see https://stackoverflow.com/questions/8915230/invalid-application-of-sizeof-to-incomplete-type-with-a-struct
    #__C__ smartstr
    $s = ph_smartstr_new("moo.txt", NULL);
    #__C__ Mixed
    $r = file_get_contents($s);
    if (COMPARE_MIXED($r, false)) {
        printf("Could not read from file\n");
    } else {
        #__C__ smartstr
        $sub = substr(GET_MIXED_STRING($r), 0, 10
            #__C__ ,NULL
        );
        printf("Sub: %s\n", GET_STRING($sub));
        //free(sub);
    }
    //ph_smartstr_free($s);
    #__C__ ph_free_mixed(&$r);

    return 0;
}
//?>
//<?php ob_end_clean(); main();
