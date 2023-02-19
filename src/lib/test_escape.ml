module Log = Dolog.Log

let%test_unit "trivial escape" =
    let source = {|<?php // @pholyglot
class Point {
    public int $x;
}
function foo(): Point {
    $p = new Point();
    return $p;
}
    |} in
    ()

let s = {|
class Thing {
    // public StringBuffer $name;
    public string $name;
}

function getName(Thing $t): string {
    // Not allowed to escape withouy copying
    // Must be copied if it's GString*
    return $t->name;
}

function foo(): void
{
    $t = new Thing();
    $t->name = "Hello";
    // Can it figure out that $name aliasing $t->name?
    $name = getName($t);
    printf("%s\n", $name);
}

function getNameClone(Thing $t): string {
    // Not valid PHP to clone non-object
    //return clone $t->name;
}

function main(): int
{
    foo();
    return 0;
}
|}

(* Always pass around pointers? *)
(* Value types vs reference types *)
(* $a = 0;  return $a; // $a escapes *)
(* Escape analysis of returning linked list *)
(* Use escape analysis to free strings that do not escape, by injecting free() before return. *)
(* Different alloc types: heap, stack, pool, depending on escape status *)
