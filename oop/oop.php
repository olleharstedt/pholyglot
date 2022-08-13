//<?php echo "\x08\x08"; ob_start(); ?>
#include <stdio.h>
#include <stdlib.h>
#include <mysql.h>
#include <glib.h>
#define function 
#define class struct
#define __PHP__ 0
#define METHOD_NAME(n) (fnptr_n)

// Stubbs
void ob_start() {}
void ob_end_clean() {}

// <?php
class PluginBase
{
#define public char*
    public $name;

#define public char*
    #REMOVEME public function (*METHOD_NAME(PluginBase_getName)) (struct PluginBase*);
#if __PHP__
    public function getName($self): string
    {
        // TODO: Duplicated function body.
        $name = "name";
        return $self->$name;
    }
#endif
};

// ?>
char* METHOD_NAME(PluginBase_getName) (struct PluginBase* $this)
{
    return $this->$name;
}

int
// <?php
function main(int $s)
{
}

// ?>
// <?php ob_end_clean(); main(1);
