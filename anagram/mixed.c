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
    struct Result r = {.t = STRING, .b = false};
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

int main()
{
    struct Result r = file_get_contents("moo");
    if (COMPARE_MIXED(false)(r, false)) {
    }
    printf("%d\n", DO_OP(10, +));
    return 0;
}
