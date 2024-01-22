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
    ArrayObject__offsetSet(ao, key, &value);
    return 0;
}
