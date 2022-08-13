//<?php echo "\x08\x08"; ob_start(); ?>
#include <stdio.h>
#include <stdlib.h>
#include <mysql.h>
#include <glib.h>
#define function 
#define class struct
#define __PHP__ 0
#define METHOD_NAME(n) (fnptr_##n)

// Stubbs
void ob_start() {}
void ob_end_clean() {}

// <?php
class PluginBase
{
#define public char*
    public $name;

    #__C__ char* (*getName) (struct PluginBase*);
// End of C struct def
#__C__ };

#if __PHP__
    public function getName($self): string
#endif
    #__C__ char* PluginBase_getName (struct PluginBase* $self)
    {
        #__C__ char*
        $name = "name";
        return $self->$name;
    }

#if __PHP__
// End of PHP class def
}
#endif

// <?php

#__C__ int
function main(int $s)
{
    $pb = new PluginBase();
    return 0;
}
// <?php ob_end_clean(); main(1);
