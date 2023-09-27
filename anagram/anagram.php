<?php

// strtok
// fgets
// TODO: Return string|false? Union struct? But not polyglot?
$words = explode("\n", file_get_contents('http://wiki.puzzlers.org/pub/wordlists/unixdict.txt'));
foreach ($words as $word) {
    // string to array, but string is already array in C
    $chars = str_split($word);
    // strsort, qsort
    sort($chars);
    // implode, but string is already an array
    // $anagram is SplDoublyLinkedList<String>
    $anagram[implode($chars)][] = $word;
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
// define("count", "count");
// array_map(count, $anagram) might work if count() is already define, and array_map expect proper function pointer
// But can use void* function pointers too
$best = max(array_map('count', $anagram));
foreach ($anagram as $ana) {
    if (count($ana) == $best) {
        // Do something else than print_r here
        print_r($ana);
    }
}