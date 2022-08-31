<?php // @pholyglot

class Point {
    public int $x;
    public int $y;
}

function main(): int {
    $p = new Point();
    $p->x = 100;
    $p->y = 200;
    printf("%d %d\n", $p->x, $p->y);
    return 0;
}
