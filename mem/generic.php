<?php

class Point {}
define("Point", "Point");
define("POOL", 0);
function __new($class) { return new $class; }
$p = __new(Point, POOL);
