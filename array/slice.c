//<?php echo "\x08\x08"; ob_start(); ?>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define array(...) {__VA_ARGS__}
#define array_make(type, i, ...) {.thing = (type[]) array(__VA_ARGS__), .length = i}
#define array_get(type, arr, i) ((type*) arr.thing)[i]
#define new(x) x ## __constructor(malloc(sizeof(struct x)))

typedef struct Body* Body;
struct Body {
    int x;
};
Body Body__constructor(Body this)
{
    this->x = 10;
    return this;
}
typedef struct array array;
struct array {
    size_t length;
    void* thing;
};

array array_slice(array old, int offset)
{
    printf("%p\n", (void*) ((Body*) old.thing)[0]);
    printf("%p\n", (void*) ((Body*) old.thing)[1]);
    printf("%d\n", (int) old.length - offset);
    array new = array_make(Body, old.length - offset, NULL);
    printf("new length %d\n", (int) new.length);
    int j = 0;
    for (int i = offset; i < old.length; i++) {
        printf("i = %d\n", i);
        //((Body*) new.thing)[j] = ((Body*) old.thing)[i];
        ((Body*) new.thing)[j] = array_get(Body, old, i);
        j++;
        //memcpy(&((Body*) new.thing)[i], &((Body*) old.thing)[i], sizeof(Body));
        //((Body*) new.thing)[i] = array_get(Body, old, i);
        //new.thing[i] = old.thing[i];
    }
    //printf("%d\n", new.thing[0]->x);
    printf("%p\n", (void*) ((Body*)new.thing)[0]);
    return new;
}

int main()
{
    array a = array_make(Body, 2, new(Body), new(Body));
    //array a = {.thing = (Body[]) {new(Body), new(Body)}, .length = 2};
    printf("%p\n", (void*) ((Body*) a.thing)[0]);
    printf("%p\n", (void*) ((Body*) a.thing)[1]);
    printf("%p\n", (void*) ((Body*) a.thing)[2]);
    //Body body1 = new(Body);
    //Body body2 = new(Body);
    /*
    array a = {
        .length = 2,
        .thing = malloc(2 * sizeof(Body))
    };
    */
    array b = array_slice(a, 1);
    printf("slice b[0]->x = %d\n", array_get(Body, b, 0)->x);
    return 0;
}
