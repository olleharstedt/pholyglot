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
function additems(SplDoublyLinkedList $list, int $nr): void
{
    $a = 0;
    do {
        // Use same memory allocation as $list
        $p = /** @alloc $list */ new Point();
        $p->x = $a;
        $p->y = $a;
        $list->push($p);
        $a++;
    } while ($a < $nr);
}

function main(): int
{
    /** @var SplDoublyLinkedList<Point> */
    $list1 = /** @alloc arena */ new SplDoublyLinkedList();
    additems($list1, 10);
    printlist($list1);

    // Defaults to Boehm GC
    /** @var SplDoublyLinkedList<Point> */
    $list2 = /** @alloc boehm */ new SplDoublyLinkedList();
    additems($list2, 10);
    printlist($list2);

    return 0;
}
