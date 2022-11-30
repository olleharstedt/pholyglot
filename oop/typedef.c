#include <stdio.h>
#include <stdlib.h>

typedef struct Point Point_struct;
typedef struct Point* Point;
struct Point
{
    int x;
    int y;
    int z;
};

int main()
{
    Point p = (Point) malloc(sizeof(Point_struct));
    p->z = 10;
    printf("z = %d\n", p->z);
    return 0;
}
