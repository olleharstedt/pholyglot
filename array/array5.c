#include <stdio.h>
#include <stdlib.h>

struct array
{
    void* thing;
    int length;
};

void foo(struct array numbers)
{
    for (int i = 0; i < numbers.length; i++) {
        printf("%d", ((int*) numbers.thing)[i]);
    }
}

int main()
{
    int _nrs[5] = {
        1, 2, 3, 4
    };
    struct array nrs = {&_nrs, 4};
    foo(nrs);
    return 0;
}
