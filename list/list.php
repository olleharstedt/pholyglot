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

/**
 * @param SplDoublyLinkedList<Point> $list
 */
function additems(SplDoublyLinkedList $list, int $nr)
{
    $a = 0;
    do {
        // Use same memory allocation as $list
        $p = /** @alloc $list */ new Point();
        $p->x = 10;
        $p->y = $a;
        $list->push($p);
        $a++;
    } while ($a < $nr);
}

function main(): int
{
    $list1 = /** @alloc arena */ new SplDoublyLinkedList();
    additems($list1, 10);
    printlist($list1);

    // Defaults to Boehm GC
    $list2 = new SplDoublyLinkedList();
    additems($list2, 20);
    printlist($list2);

    return 0;
}
