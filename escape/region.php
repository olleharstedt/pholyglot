<?php

namespace Pholly {
    // Must only have private props?
    // Must not escape any props?  All items are freed the Pool object goes out of scope 
    interface Pool
    {
    }

    // Polyfill over GString
    class GString {
        private $s;
        public function __construct($s) { $this->s = $s; }
        public function append($s) { $this->s .= $s; }
        public function get() { return $this->s; }
    }
}

namespace App {
    class MyThings implements \Pholly\Pool
    {
        private $stuff = [];

        /** @var char[100] */
        public string $name;

        public function addThing($e)
        {
            $this->stuff[] = $e;
        }

        public function calc(): int
        {
            $result = 0;
            foreach ($this->stuff as $e) {
                $result += $e;
            }
            return $result;
        }
    } 

    $mt = new MyThings();
    $mt->name = 'a';
}
