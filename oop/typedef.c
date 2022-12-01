#include <stdio.h>
#include <stdlib.h>

#define new(x) alloca(sizeof(struct Point))

typedef struct Point* Point;
struct Point
{
    int x;
    int y;
    int z;
};

int main()
{
    // TODO: Or sizeof Point without ()
    // 22:45 < candide> T *p = malloc(sizeof *p) is more robust than T *p = malloc(sizeof (T)) because sizeof *p will always be the correct size of T and it prevents unnecessarily repeating the T
    // (DRY principle). If sizeof (T) is used and later one of the T's is changed there will be a mismatch. T *p = malloc(sizeof *p) is more concise and easier to read, especially
    // when T is a compound type.
    Point p = new(Point);
    p->z = 10;
    printf("z = %d\n", p->z);
    return 0;
}
