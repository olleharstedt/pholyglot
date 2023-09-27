#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define COMPARE_STRING(res, val) strcmp(res.string, val)
#define COMPARE_INT(res, val) res.b == val
#define COMPARE_MIXED(res, val) _Generic(val, \
    char*: COMPARE_STRING(res, val),\
    int: COMPARE_INT(res, val)\
    )(val)

union Result
{
    char* string;
    bool b;
};

// TODO: Should be able to return false
char* file_get_contents(char* filename)
{
}

int main()
{
    union Result r = {.string = "", .b = true};
    if (COMPARE_MIXED(&r, true)) {
        printf("Hello\n");
    }
    return 0;
}
