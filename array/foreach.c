//<?php echo "\x08\x08"; ob_start(); ?>

#include <stdio.h>

#define AS ,
#define FIRST_ARG_(N, ...) N
#define FIRST_ARG(args) FIRST_ARG_ args
#define foreach(...) for(int i = 0;i < count(FIRST_ARG((__VA_ARGS__))); i++)

int count(void* a) { return 10; }

//<?php
#define function int
function main()
{
    //?>
    int//<?php
    $i = 0;
    for (; $i < 10; $i++) {
    }

    double x[] = {1.1, 2.2, 3.3};
    int y;

    for (double i = 0, f = 0; i < 3; i++, f = x[(int) i]) {
        printf("%f\n", f);
    }

    //foreach(x AS y) {
        //int y = x[i];
    //}

    return 0;
}
//<?php main(); ob_end_clean();
