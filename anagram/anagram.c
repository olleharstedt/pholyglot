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

//#define COMPARE_STRING(res, val) strcmp(res.string.str, val.str)
//#define COMPARE_INT(res, val) (res.b == val)
//#define COMPARE_MIXED(val) _Generic(val, \
//smartstr: compare_string,\
//int: compare_int\
//)


bool compare_string(Mixed r, smartstr val)
{
    printf("compare_string\n");
    return strcmp(r.s->str, val->str) == 0;
}

#define DO_OP(a, op) a op a
#define COMPARE_MIXED(res, op, val) _Generic(val,\
    int: (res.t == BOOL && res.b op val),\
    bool: (res.t == BOOL && res.b op val),\
    char*: (res.t == STRING && strncmp(res.s->str, val, res.s->len) == 0),\
    default: false\
    )

#define OP_EQUALS ==
#define OP_PLUS +

int compare_int(Mixed res, int val) {
    printf("compare_int\n");
    return res.b == val;
}

/**
 * Compile with:
 *   gcc -g -I. -Wno-incompatible-pointer-types -xc -fsanitize=undefined -fsanitize=address -lgc anagram.c
 *   gcc -g -I. -Wno-incompatible-pointer-types -xc -lgc anagram.c
 */
int main()
{
    // @see https://stackoverflow.com/questions/8915230/invalid-application-of-sizeof-to-incomplete-type-with-a-struct
    smartstr s = malloc(sizeof(struct _smartstr));
    s->str = malloc(sizeof(char) * 8);
    strncpy(s->str, "moo.txt", 8);
    s->len = 9;
    Mixed $r = file_get_contents(s);
    if (COMPARE_MIXED($r, OP_EQUALS, true)) {
        printf("Hello\n");
    }
    free($r.s);
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
