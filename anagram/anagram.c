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


/**
 * Compile with:
 *   gcc -g -I. -Wno-incompatible-pointer-types -xc -fsanitize=undefined -fsanitize=address -lgc anagram.c
 *   gcc -g -I. -Wno-incompatible-pointer-types -xc anagram.c -lgc
 *   gcc -xc -lgc -I. -I/usr/include/glib-2.0 -I/usr/lib/x86_64-linux-gnu/glib-2.0/include/ anagram.c
 */
#define function int
function main()
#undef function
{
    // @see https://stackoverflow.com/questions/8915230/invalid-application-of-sizeof-to-incomplete-type-with-a-struct
    smartstr
    $s = ph_smartstr_new("moo.txt", NULL);
    Mixed $r = file_get_contents($s);
    if (COMPARE_MIXED($r, false)) {
        printf("Could not read from file\n");
    } else {
        char* sub = malloc(51);
        strncpy(sub, $r.s->str, 49);
        sub[49] = '\0';
        printf("Big blob: %s\n", sub);
        free(sub);
    }
    ph_smartstr_free($s);
    ph_free_mixed(&$r);

    /*
    Mixed $r2;
    smartstr $s1 = {.str = "moo", .len = 3};
    $r2.s = $s1;
    smartstr $s2 = {.str = "moo", .len = 3};
    //if (COMPARE_MIXED(s2)(r2, s2)) {
        //printf("Hello 2\n");
    //}

    Mixed $r3 = file_get_contents("moo\0");
    if (COMPARE_MIXED($r3, OP_EQUALS, false)) {
        printf("Is false\n");
    } else if (COMPARE_MIXED($r3, OP_EQUALS, "asd\0")) {
        printf("Is asd\n");
    }
    */

    return 0;
}
