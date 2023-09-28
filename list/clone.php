<?php // @pholyglot

class Id
{
    public int $value;
}

class User
{
    public Id $id;
}

function main(): int
{
    $user = new User();
    $user->id = new Id();
    $id = $user->id;
    $id->value = 10;
    printf("%d\n", $id->value);
    return 0;
}
