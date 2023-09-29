<?php // @pholyglot

class User
{
    public int $id;
}

// ../src/_build/install/default/bin/pholyglot clone.php | sed -e "s/#__C__//g" | gcc -xc - -lgc -I. -I/usr/include/glib-2.0 -I/usr/lib/x86_64-linux-gnu/glib-2.0/include/
function main(): int
{
    $user1 = new User();
    $user1->id = 10;
    $user2 = clone $user1;
    printf("%d\n", $user2->id);
    return 0;
}
