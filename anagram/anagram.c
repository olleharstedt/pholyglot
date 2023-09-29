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
    smartstr: compare_string,\
    int: compare_int\
	)

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

typedef struct _Result Result;
struct _Result
{
    enum type t;
    union {
        smartstr s;
        bool  b;
    };
    // TODO: Field for custom types
};

int compare_string(Result res, smartstr val) {
    printf("compare_string\n");
    printf("%s\n", res.s.str);
    printf("%s\n", val.str);
    return strcmp(res.s.str, val.str) == 0;
}

int compare_int(Result res, int val) {
    printf("compare_int\n");
    return res.b == val;
}

Result file_get_contents(char* filename)
{
    printf("file_get_contents\n");
    Result r;
    if (strcmp(filename, "moo") == 0) {
        r.b = false;
    } else {
        smartstr* sm = malloc(sizeof(smartstr));
        sm->str = malloc(4);
        sm->len = 4;
        strcpy(sm->str, "asd");
        r.s = *sm;
    }
    return r;
}

/**
 * Compile with:
 */
int main()
{
    Result r;
    r.b = true;
    if (COMPARE_MIXED(true)(r, true)) {
        printf("Hello\n");
    }
    Result r2;
    smartstr s1 = {.str = "moo", .len = 3};
    r2.s = s1;
    //r2.string = {.str = "moo", .len = 3};
    smartstr s2 = {.str = "moo", .len = 3};
    if (COMPARE_MIXED(s2)(r2, s2)) {
        printf("Hello 2\n");
    }

    Result r3 = file_get_contents("moo");
    if (COMPARE_MIXED(false)(r3, false)) {
    }

    return 0;
}
