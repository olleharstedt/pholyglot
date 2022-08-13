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

struct PluginBase
{
    char* name;
    char* (*fncPtr_getName)(struct PluginBase*);
};

int
// <?php
function main(int $s)
{
}

// ?>
// <?php ob_end_clean(); main(1);
