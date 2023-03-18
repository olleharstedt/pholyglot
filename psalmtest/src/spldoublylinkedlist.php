<?php

class Point
{
    public int $x = 1;
    public int $y = 2;
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
