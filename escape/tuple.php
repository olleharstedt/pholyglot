<?php

use _ as tuple;
use _ as vect;
use _ as list_;  // ??
use _ as hashtbl;

class _
{
    public function _()
    {
        echo 'hey';
    }
}

function foo(array|tuple $t): array|tuple
{
    return [$t[0], "asd"];
}

//foo(tuple([1, "asd"]));
[$a, $b] = foo([0, 2]);
echo $a;
echo $b;

$_ = new _();
$_->_();
