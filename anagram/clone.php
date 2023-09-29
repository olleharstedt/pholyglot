<?php // @pholyglot

class User
{
    public int $id;
}

// ../src/_build/install/default/bin/pholyglot clone.php | sed -e "s/#__C__//g" | gcc -O1 -I. -Wno-incompatible-pointer-types -xc - -lgc
function main(): int
{
    $user1 = new User();
    $user1->id = 10;
    $user2 = clone $user1;
    printf("%d\n", $user2->id);
    return 0;
}
