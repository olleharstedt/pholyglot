//<?php echo "\x08\x08"; ob_start(); ?>
#include <stdio.h>
#include <stdlib.h>
#include <mysql.h>
#include <glib.h>
#define function 
#define class struct

// Stubbs
void ob_start() {}
void ob_end_clean() {}

// Wrap C functions to look like PHP.
MYSQL* mysqli_connect()
{
    MYSQL* $connection = mysql_init(NULL);
    if (!mysql_real_connect($connection, "localhost", "olle", "password", "db", 0, NULL, 0)) {
        fprintf(
            stderr,
            "Failed to connect to database: Error: %s\n",
            mysql_error($connection)
        );
    }
    return $connection;
}

MYSQL_RES* mysqli_query(MYSQL* $connection, char* query)
{
    // TODO: Defensive programming
    mysql_query($connection, query);
    MYSQL_RES* result = mysql_store_result($connection);
    return result;
}

MYSQL_ROW mysqli_fetch_row(MYSQL_RES* res)
{
    return mysql_fetch_row(res);
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
    // TODO: Possible to do with macro instead of wrapping function?
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
    MYSQL_RES* // <?php 
    $result = mysqli_query($connection, "SELECT username FROM users WHERE id = 1");
    // ?> 
    MYSQL_ROW // <?php
    $row = mysqli_fetch_row($result);
    // ?>
    char*
    // <?php
    $username = $row[0];

    // ?>
    float // <?php
    $t = 1.0;
    ob_end_clean();
    printf("Point data: x = %d, y = %d\n", $p->$x, $p->$y);
    printf("Hello %f\n", $s + $t);
    printf("Hello %d\n", $arr[0]);
    printf("Username: %s\n", $username);
}

// ?>
// <?php ob_end_clean(); main(1);
