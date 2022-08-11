//<?php echo "\x08\x08"; ob_start(); ?>
#include <stdio.h>
#include <stdlib.h>
#include <mysql.h>
#define function 
#define class struct
#define new 

// Stubbs
void ob_start() {}
void ob_end_clean() {}

MYSQL* mysqli_connect()
{
    // TODO: Missing defensive programming
    MYSQL* $connection = mysql_init(NULL);
    mysql_real_connect($connection, "localhost", "user", "password", "db", 0, NULL, 0);
    return $connection;
}

// <?php

class Point
{
#define public int
    public $x;
#define public int
    public $y;
};

// ?>

// <?php function new_Point() { return new Point; } // <?php

// ?>

struct Point* new_Point() {
    return (struct Point*) malloc(sizeof(struct Point));
}

int
// <?php
function main(int $s)
{
    ob_start();
    // Example of int array
    // TODO: How to init with [1, 2, 3]? {1, 2, 3} in C. :(
    // ?>
    int // <?php $arr = [5 => 0];
    $arr[5];
    $arr[0] = 1;
    $arr[1] = 2;
    $arr[2] = 3;

    // Example of point struct
    // ?>
    struct Point* // <?php
    $p = new_Point();
    // ?>
    char* // <?php
    $x = "x";
    // ?>
    char* // <?php
    $y = "y";

    // Database test
    // ?>
    MYSQL*
    // <?php
    $connection = mysqli_connect("localhost", "olle", "password", "db");

    // ?>
    float // <?php
    $t = 1.0;
    ob_end_clean();
    printf("Point data: x = %d, y = %d\n", $p->$x, $p->$y);
    printf("Hello %f\n", $s + $t);
    printf("Hello %d\n", $arr[0]);
}

// ?>
// <?php ob_end_clean(); main(1);
