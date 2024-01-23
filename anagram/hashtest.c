#include "phollylib.c"

/**
 * Compile like:
 *   gcc hashtest.c -lgc -lm -g
 */
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
