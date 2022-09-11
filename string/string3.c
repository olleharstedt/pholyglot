#include <stdio.h>
#include <stdlib.h>
#include <time.h>

struct Point {
    int x;
    char name[100];
};

struct Point new()
{
    int r = (int) rand() % 10 + 1;
    printf("%d\n", r);
    char n[r];
    for (int i = 0; i < r; i++) {
        n[i] = 'a';
    }

    n[r] = '\0';

    struct Point p = {
        10,
        n
    };
    return p;
}

int main()
{
    time_t t;
    srand((unsigned) time(&t));
    struct Point p = new();
    printf("%s\n", p.name);
    return 0;
}
