#include <stdio.h>
#include <string.h>
#include <stdbool.h>

enum type
{
    STRING = 0,
    BOOL   = 1
};

typedef struct _Mixed Mixed;
struct _Mixed
{
    enum type t;
    union {
        char* s;
        bool  b;
    };
};

#define COMPARE_MIXED(mixed, val) _Generic(val,\
    int: (mixed.t == BOOL && mixed.b == val),\
    bool: (mixed.t == BOOL && mixed.b == val),\
    char*: (mixed.t == STRING && strcmp(mixed.s, val) == 0)\
    )

int main() {
    _Bool a = false;
    int c = _Generic(a, int: 1, double: 2, bool: a == 0);
    printf("c = %d\n", c);

    Mixed m = {.t = BOOL, .b = true};
    //Mixed m = {.t = STRING, .s = "hello"};
    if (COMPARE_MIXED(m, false)) {
        printf("m is boolean false\n");
    } else if (COMPARE_MIXED(m, true)) {
        printf("m is boolean true\n");
    } else if (COMPARE_MIXED(m, "hello")) {
        printf("m is string 'hello'\n");
    }

    return 0;
}
