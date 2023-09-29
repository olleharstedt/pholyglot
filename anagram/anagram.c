#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

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

typedef struct _smartstr smartstr;
struct _smartstr
{
    char* str;
    long len;
};

enum type
{
    STRING = 0,
    BOOL   = 1
};

typedef struct _Result Mixed;
struct _Result
{
    enum type t;
    union {
        smartstr s;
        bool  b;
    };
    // TODO: Field for custom types
};

bool compare_string(Mixed r, smartstr val)
{
    printf("compare_string\n");
    return strcmp(r.s.str, val.str) == 0;
}

#define DO_OP(a, op) a op a
#define COMPARE_MIXED(res, op, val) _Generic(val,\
    int: (res.t == BOOL && res.b op val),\
    char*: (res.t == STRING && strncmp(res.s.str, val, res.s.len) == 0)\
    )

#define OP_EQUALS ==
#define OP_PLUS +

int compare_int(Mixed res, int val) {
    printf("compare_int\n");
    return res.b == val;
}

Mixed file_get_contents(char* filename)
{
    printf("file_get_contents\n");
    Mixed r;
    if (strcmp(filename, "moo\0") != 0) {
        r.t = BOOL;
        r.b = false;
    } else {
        smartstr* sm = malloc(sizeof(smartstr));
        sm->str = malloc(5);
        sm->len = 5;
        strcpy(sm->str, "asd\0");
        r.s = *sm;
        r.t = STRING;
    }
    return r;
}

/**
 * Compile with:
 *   gcc -g -I. -Wno-incompatible-pointer-types -xc -fsanitize=undefined -fsanitize=address -lgc anagram.c
 *   gcc -g -I. -Wno-incompatible-pointer-types -xc -lgc anagram.c
 */
int main()
{
    Mixed $r;
    $r.t = BOOL;
    $r.b = true;
    if (COMPARE_MIXED($r, OP_EQUALS, true)) {
        printf("Hello\n");
    }
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

    return 0;
}
