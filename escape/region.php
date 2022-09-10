<?php

namespace Pholly {
    // Must only have private props?
    // Must not escape any props?
    // All items are freed the Pool object goes out of scope
    interface Pool
    {
    }

    class StringBuffer {
        private $s;
        public function __construct($s) { $this->s = $s }
        public append($s) { $this->s .= $s; }
        public get() { return $this->s; }
    }
}

class MyThings implements Pholly\Pool
{
    private $stuff = [];
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
