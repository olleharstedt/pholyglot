#include <stdio.h>
#define EVALUATE(X) _Generic((X), _Bool : "boolean", default : "not boolean")

int main()
{
    printf("%s\n", EVALUATE(5));
    return 0;
}
