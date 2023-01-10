#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
typedef struct array array;
struct array { uintptr_t* thing; size_t length; };
array array_slice(array old, int offset)
{
    size_t new_length = old.length - offset;
    if (new_length < 1) {
        return (array) {.length = 0, .thing = NULL};
    }
    printf("%ld\n", new_length);
    array new = {
        .length = new_length,
        .thing = malloc(sizeof(uintptr_t) * new_length)
    };
    size_t j = 0;
    for (size_t i = offset; i < old.length; i++) {
        new.thing[j] = old.thing[i]; j++;
    }
    return new;
}
