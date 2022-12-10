#include <stdio.h>
#include <stdlib.h>

#define array(args...) {args}

typedef struct array array;
struct array
{
    void* thing;
    size_t length;
};

void foo(array $numbers)
{
    #__C__ int
    $i = 0;
    for ($i = 0; $i < $numbers.length; $i++) {
        printf("%d", (
            #__C__ (int*)
            $numbers.thing
            )[$i]
        );
    }
}

#define function int
function main()
{
    #__C__ int 
    $_nrs
    #__C__ [5]
    = array(
        1, 2, 3, 4
    );
    array $nrs = {&$_nrs, 4};
    foo($nrs);
    return 0;
}
