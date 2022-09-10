<?php

// Must only have private props?
// Must not escape any props?
// All items are freed the Pool object goes out of scope
class Pool
{
    public $list;

    public addItem($e)
    {
        $this->list[] $e;
    }
}
