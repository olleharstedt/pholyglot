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
    #__C__ void (*setName) (struct PluginBase*, char*);
// End of C struct def. Class methods are outside the struct.
#__C__ };

#if __PHP__
    public function getName(PluginBase $self): string
#endif
    #__C__ char* PluginBase_getName (struct PluginBase* $self)
    {
#if __PHP__
        $name = "name";
#endif
        return $self->$name;
    }

#if __PHP__
    public function setName(PluginBase $self, string $newName): void
#endif
    #__C__ void PluginBase_setName (struct PluginBase* $self, char* $newName)
    {
#if __PHP__
        $name = "name";
#endif
        $self->$name = $newName;
    }

#if __PHP__
// End of PHP class def.
}
#endif

// ?>

struct PluginBase* new_PluginBase()
{
    struct PluginBase* pb = malloc(sizeof(struct PluginBase));
    pb->getName = &PluginBase_getName;
    pb->setName = &PluginBase_setName;
    return pb;
}

// <?php

#if __PHP__
function new_PluginBase()
{
    return new PluginBase();
}
#endif

#__C__ int
function main()
{
    #__C__ char*
    $newName = "MyNicePlugin";

    #__C__ struct PluginBase*
    $pb = new_PluginBase();
    $pb->setName($pb, $newName);

#if __PHP__
    $name = "name";
#endif

    printf("Hello, my plugin name is %s\n", $pb->getName($pb));

    return 0;
}
// ?>
// <?php ob_end_clean(); main(1);
