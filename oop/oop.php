//<?php echo "\x08\x08"; ob_start(); ?>
#include <stdio.h>
#include <stdlib.h>
#include <mysql.h>
#include <glib.h>
#define function 
#define class struct
#define __PHP__ 0

// <?php

class PluginBase
{
    public $name;
    public function getName(): string
    {
        return $this->name;
    }
}

class MyPlugin extends PluginBase
{
    public function doTheThing()
    {
        printf("Doing the thing\n");
    }
}

$myPlugin = new MyPlugin();
printf("Plugin name: %s", $myPlugin->getName());
$myPlugin->doTheThing();
