//<?php echo "\x08\x08"; ob_start(); ?>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

enum type
{
    STRING = 0,
    BOOL   = 1
};

struct Result
{
    enum type t;
    union {
        char* str;
        bool  b;
    };
};

struct Result file_get_contents(char* filename)
{
    struct Result r = {.t = BOOL, .b = false};
    return r;
}

#define COMPARE_MIXED(val) _Generic(val, \
    char*: compare_string,\
    int: compare_int\
	)

bool compare_string(struct Result r, char* val)
{
    printf("compare_string\n");
    return false;
}

bool compare_int(struct Result r, int val)
{
    printf("compare_int\n");
    return false;
}

#define DO_OP(a, op) a op a
#define COMPARE(res, val, op) _Generic(val,\
    int: (res.t == BOOL && res.b op val)\
    )

#define OP_EQUALS ==
#define OP_PLUS +

#if __PHP__//<?php
define("OP_EQUALS", "==");
define("OP_PLUS", "+");
function compare($res, $val, $op)
{
    switch ($op) {
        case OP_EQUALS:
            return $res == $val;
            break;
        default:
            assert(false, 'Unkown operation');
    }
}
#endif
//<?php
/**
 * Compile with
 *   cat mixed.c | sed -e "s/#__C__//g" | gcc -g -I. -Wno-incompatible-pointer-types -xc - -lgc
 */
#define function int
function main()
#undef function
{
    #__C__ struct Result
    $r = file_get_contents("moo");
    if (COMPARE($r, false, OP_EQUALS)) {
        printf("Is false 2\n");
    }
    return 0;
}
//?>
//<?php ob_end_clean(); main();
