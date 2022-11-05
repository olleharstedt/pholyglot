<?php // @pholyglot

class Point
{
    public int $x;
    public function getX(): int
    {
        printf("Mooo");
        return $this->x;
    }
}

function main(): int
{
    printf("Hello, world!\n");
    return 0;
}
