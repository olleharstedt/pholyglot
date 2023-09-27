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
#define COMPARE_MIXED(val) _Generic(val, \
    struct smartstr: compare_string,\
    char*: compare_string,\
    int: compare_int\
	)

struct smartstr
{
    char* str;
    long len;
};

union Result
{
    struct smartstr string;
    bool b;
};

int compare_string(union Result res, struct smartstr val) {
    return 0;
}

int compare_int(union Result res, int val) {
    return 0;
}

// TODO: Should be able to return false
char* file_get_contents(char* filename)
{
}

int main()
{
    //union Result r;
    //r.b = true;
    //if (COMPARE_MIXED(r, true)) {
        //printf("Hello\n");
    //}
    union Result r2;
    //r2.string = {.str = "moo", .len = 3};
    struct smartstr s2 = {.str = "moo", .len = 3};
    if (COMPARE_MIXED(s2)) {
        printf("Hello 2\n");
    }
    return 0;
}
