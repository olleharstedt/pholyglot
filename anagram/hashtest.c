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

int main()
{
    //GC_INIT();
    //smartstr s1 = heap_mem.alloc(NULL, sizeof(struct _smartstr));
    //s1->t       = SMART_STRING;
    //s1->len     = 12;
    //s1->str     = (char*) heap_mem.alloc(NULL, 12);
    //strcpy(s1->str, "Hello world\0");

    smartstr s1 = smartstr_of_chars("Hello world\0", &heap_mem);

    ArrayObject ao = ArrayObject__constructor(new(ArrayObject, heap_mem), heap_mem);
    
    ArrayObject__offsetSet(ao, "foo", (void*) s1);
    smartstr s2 =ArrayObject__offsetGet(ao, "foo");
    printf("Smart string: %s\n", s2->str);

    return 0;
}
