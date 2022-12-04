<?php

class Point { }
define("Point", "Point");
$a = 20;
function foo() { return "Point"; }
$o = new ($a = foo());
