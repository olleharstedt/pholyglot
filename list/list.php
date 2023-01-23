<?php

define("SplDoublyLinkedList", "SplDoublyLinkedList");
$list = new(SplDoublyLinkedList);
$list->push(10);
$list->push(20);

foreach ($list as $item) {
    echo $item;
}
