//<?php echo "\x08\x08"; ob_start(); ?>
#include <stdio.h>
#define function 
#define class struct
#define new 

// <?php

#define public int
class Point
{
    public $x;
    public $y;
};

// <?php function new_point() { return new Point; } // <?php

// ?>

struct Point new_point() {
    return (struct Point) {};
}

int
// <?php
function main(int $x)
{
    // Example of int array
    // ?>
    int // <?php
    $arr[5];
    $arr[0] = 1;
    $arr[1] = 2;
    $arr[2] = 3;

    // Example of point struct
    // ?>
    struct Point // <?php
    $p = new_point();
    printf("Point data: x = %d, y = %d\n", $p.$x, $p.$y);

    // ?>
    float // <?php
    $y = 1.0;
    printf("Hello %f\n", $x + $y);
    printf("Hello %d\n", $arr[0]);
}

// ?>
// <?php ob_end_clean(); main(1);
