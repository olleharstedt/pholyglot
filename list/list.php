<?php // @pholyglot

class Point
{
    public int $x;
    public int $y;
}

/**
 * @param SplDoublyLinkedList<Point> $list
 */
function printlist(SplDoublyLinkedList $list): void
{
    foreach ($list as $item) {
        printf("Point x = %d\n", $item->x);
    }
}
