#include "phollylib.c"

/**
 * Compile like:
 *   gcc hashtest.c -lgc -lm -g
 * or
 *   gcc -Wno-incompatible-pointer-types hashtest.c -lgc -lm -g
 */
/*
int main()
{
    //GC_INIT();
    ArrayObject ao = ArrayObject__constructor(new(ArrayObject, heap_mem), heap_mem);
    char* key = "foo\0";
    const int value = 10;

    char* key2 = "bar\0";
    const int value2 = 20;

    ArrayObject__offsetSet(ao, key, &value);
    ArrayObject__offsetSet(ao, key2, &value2);
    const int* val = ArrayObject__offsetGet(ao, key);
    printf("Val = %d\n", *val);
    val = ArrayObject__offsetGet(ao, "bar");
    printf("Val = %d\n", *val);
    return 0;
}
*/

void foo()
{
    //GC_INIT();
    //smartstr s1 = heap_mem.alloc(NULL, sizeof(struct _smartstr));
    //s1->t       = SMART_STRING;
    //s1->len     = 12;
    //s1->str     = (char*) heap_mem.alloc(NULL, 12);
    //strcpy(s1->str, "Hello world\0");
    Arena __a = malloc(sizeof(struct Arena));
    arena_init(__a, malloc(256), 256);
    arena_mem.alloc = &arena_alloc;
    arena_mem.arena = __a;

    struct mem m = arena_mem;

    smartstr s1 = smartstr_of_chars("Hello world\0", &m);
    smartstr s2 = smartstr_of_chars("Hello foo bar\0", &m);

    ArrayObject ao = ArrayObject__constructor(new(ArrayObject, m), m);
    
    ArrayObject__offsetSet(ao, "foo", (void*) s1);
    ArrayObject__offsetSet(ao, "bar", (void*) s2);


    smartstr s3 =ArrayObject__offsetGet(ao, "bar");
    printf("Smart string: %s\n", s3->str);

    arena_free(m.arena);
}

int main()
{
    foo();
    return 0;
}
