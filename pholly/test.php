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
    $p = new Point();
    $p->x = 10;
    printf("%d", $p->getX());
    return 0;
}
