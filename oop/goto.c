#include <stdio.h>
#include <stdlib.h>

void foo(char* world)
{
    __asm__(
        "my_hack:\n"
    );
    printf("Hello %s\n", world);
    exit(0);
}

int main()
{
    __asm__(
        "jmp  my_hack \n"
    );
    return 0;
}
