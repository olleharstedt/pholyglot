<?php // @pholyglot

// Compile with ../src/_build/default/bin/main.exe anagram.php
// Taken from https://rosettacode.org/wiki/Anagrams#PHP

// strtok
// fgets
// curl to get url content to string
// TODO: Return string|false? Union struct? But not polyglot?
// TODO: Must malloc third-party or core lib memory, so free at end of scope
// TODO: Insert free() before every return. inject_before_return recursively
//$contents = file_get_contents('http://wiki.puzzlers.org/pub/wordlists/unixdict.txt');
function STRING($s) { return $s; }
$s = STRING("moo.txt");
$contents = file_get_contents('moo.txt');
if ($contents === false) {
    return;
}
// TODO: Explode into fixed-size array or SplDoublyLinkedList?
$words = explode("\n", $contents);
// TODO: NEW FEATURE 
$anagram = new ArrayObject();
foreach ($words as $word) {
    // string to array, but string is already array in C
    $chars = str_split($word);
    // strsort, qsort
    sort($chars);
    // implode, but string is already an array
    // $anagram is SplDoublyLinkedList<String>
    // TODO: This needs a hash map implementation. Or set.
    $string = implode($chars);
    if (!isset($anagram[$string])) {
        $anagram[$string] = new SplDoublyLinkedList();
        $anagram[$string]->push($word);
    } else {
        $bucket = $anagram[$string];
        $bucket[] = $word;
    }
    //$anagram->push($word);
}

// No stdlib map
/*
int *map(int *array, int size, int (*fn)(int)) {
  int *new_array = malloc(size * sizeof(int));
  for (int i = 0; i < size; i++) {
    new_array[i] = fn(array[i]);
  }
  return new_array;
}
*/
// fmax from math.h
define("count", "count");
// array_map(count, $anagram) might work if count() is already define, and array_map expect proper function pointer
// But can use void* function pointers too
foreach ($anagram as $an) {
    $best = max($best, count($an));
}
//$best = max(array_map("count", $array));
foreach ($anagram as $an) {
    if (count($an) == $best) {
        // Do something else than print_r here
        print_r($an);
    }
}
