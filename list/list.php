<?php

define("SplDoublyLinkedList", "SplDoublyLinkedList");
$list = new(SplDoublyLinkedList);
$list->push(10);
$list->push(20);
$list->push(30);

foreach ($list as $item) {
    echo $item;
}

for ($i = 0; $i < count($list); $i++) {
    //echo $list->get($i);
}

iterator_apply($list, function () { echo 'hej'; });
