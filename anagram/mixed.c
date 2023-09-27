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
    //struct Result r = {.t = BOOL, .b = false};
    char* s = malloc(4);
    strcpy(s, "hej\n");
    struct Result r = {.t = STRING, .str = s};
    return r;
}

bool compare_string(struct Result r, char* val)
{
    printf("compare_string\n");
    return false;
}

#define DO_OP(a, op) a op a
#define COMPARE_MIXED(res, val, op) _Generic(val, int: (res.t == BOOL && res.b op val),\
    char*: (res.t == STRING && strcmp(res.str, val) == 0)\
    )

#define OP_EQUALS ==
#define OP_PLUS +

#if __PHP__//<?php
define("OP_EQUALS", "==");
define("OP_PLUS", "+");
function COMPARE_MIXED($res, $val, $op)
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
    $r = file_get_contents("moo.txt");
    if (COMPARE_MIXED($r, false, OP_EQUALS)) {
        printf("Is false\n");
    } else if (COMPARE_MIXED($r, "hej\n", OP_EQUALS)) {
        printf("Is hej\n");
    }
    return 0;
}
//?>
//<?php ob_end_clean(); main();
