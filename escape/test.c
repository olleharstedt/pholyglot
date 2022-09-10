//<?php echo "\x08\x08"; ob_start(); ?>
#include <stdio.h>
#include <stdlib.h>
#include <glib.h>

struct Thing {
    GString* name;
};

//function g_string_new(string $str) { return new GString($str); }
//function g_string_append(GString $s1, string $s2) { return new GString($s1->str . $s2); }

GString* get_name(struct Thing* t)
{
    return g_string_new(t->name->str);
}

void foo()
{
    struct Thing* t = alloca(sizeof(struct Thing));
    t->name = g_string_new("Hello");
    GString* name = get_name(t);
    printf("%s\n", name->str);
    // return t;
}

int main()
{
    foo();
    return 0;
}
