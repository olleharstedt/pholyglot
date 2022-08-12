//<?php echo "\x08\x08"; ob_start(); ?>
#include <stdio.h>
#include <stdlib.h>
#include <mysql.h>
#include <glib.h>
#define function 
#define class struct
#define __PHP__ 0

// Stubbs
void ob_start() {}
void ob_end_clean() {}

// Wrap C functions to look like PHP.
MYSQL* mysqli_connect(char* host, char* username, char* password, char* database_name)
{
    MYSQL* $connection = mysql_init(NULL);
    if (!mysql_real_connect($connection, host, username, password, database_name, 0, NULL, 0)) {
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

struct database_config {
    gchar* username;
    gchar* password;
    gchar* host;
    gchar* database_name;
};

// Wrap GLib key file functions to look like PHP.
struct database_config* parse_ini_file(char* filename)
{
    GKeyFile* key_file = g_key_file_new();
    GError** error = NULL;
    GKeyFileFlags flags;
    g_key_file_load_from_file(key_file, filename, flags, error);

    struct database_config* config = malloc(sizeof(struct database_config));
    config->username = g_key_file_get_value(key_file, "database", "username", error);
    config->password = g_key_file_get_value(key_file, "database", "password", error);
    config->host = g_key_file_get_value(key_file, "database", "host", error);
    config->database_name = g_key_file_get_value(key_file, "database", "database_name", error);

    return config;
}

gchar* ini_file_get_value(GKeyFile *key_file, const gchar *group_name, const gchar *key)
{
    GError** error = NULL;
    return g_key_file_get_value( key_file, group_name, key, error);
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

    // Read from ini config file test
    // ?>
    struct database_config* // <?php
    $config = 
#if __PHP__
    (object)
#endif
    parse_ini_file("config.ini");

    // Database test
    // ?>
    MYSQL*
    // <?php
    $connection = mysqli_connect($config->host, $config->username, $config->password, $config->database_name);
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
