## Blog posts

For information about the project, see the following blog posts:

* http://olleharstedt.github.io/programming/2022/12/12/nbody-benchmark-in-php-c-polyglot-pholyglot.html
* http://olleharstedt.github.io/programming/2022/06/11/pholyglot-php-to-php-c-transpiler.html

## IRC

    ,cc int $a = 10; printf("%d", $a); // Now I'm curious
    http://www.iso-9899.info/n1570.html#J.5.2p1 [Specialized identifiers] Characters other than the underscore _, letters, and digits, that are not part of the basic source
                     character set (such as the dollar sign $, or characters in national character sets) may appear in an identifier (6.4.2).
    ,cc int \u0024a = 10; // this, on the other hand, is strictly conforming
    "Before C++11, C++ didn't allow Unicode escapes with a code point under 0xA0, except for 0x24 ($), 0x40 (@), and 0x60 (`)."
    "C still has the same restrictions as C++98."

    -w #include <dlfcn.h> void callbyname(const char *name, const char *arg) { ((int (*)(const char *)) dlsym(RTLD_DEFAULT, name))(arg); } callbyname("puts", "YOLO");

    olle: for your c target, probably easiest to say: int *temp = &a->b; *temp = 1
    iow the code generator treats lvalues uniformly as pointers

## Notes

    #define \u0024 $
    #define \u003C ?
    #define \u002F /

    #define EMPTY
    EMPTY#ifdef

    GOTO here;
    here:

    gcc polyglot.c -I/usr/include/mysql -I/usr/include/mysql/mysql -lmysqlclient

    #include "fcgi_stdio.h"

    $a = parse_int($_GET['used_id']);

    // TODO: This won't deal with different mem strategies
    // BUT: Use _Generic to switch on it? Use same new() with different behaviour - alloca, malloc, Boehm etc.
    // BUT: Should be able to use one class with many diff allocation strats, /** @alloc stack */ etc
    // NB: new (foo()) is valid in PHP when foo() returns string/class name
    #define new(x) x ## __constructor(alloca(sizeof(struct x)))

`g_printf` vs `printf`?

`g_slice` instead of malloc?

https://stackoverflow.com/questions/25144963/converting-a-mysql-result-into-a-json-string-in-c/25523088#25523088

https://stackoverflow.com/questions/5451913/how-to-retrieve-form-post-data-via-cgi-bin-program-written-in-c

* Class can be struct.
    * Pass around `$__self` explicitly
* String can use smart strings from PHP core or GNU libc
* Array can use same array.
** `#define array const Body*` but also need to pass length as an argument
* Hashtable gonna be hard. Must use class for that?
* GC with Boehm, no long-running scripts? Or glib slice/manual ref count
    * libgc-dev on Ubuntu
* Need docblock for arrays - no collection class built-in in PHP
    * Fixed-size array (as in C); can't change size (at least not without copy mem; but mess up pointers?)
    * Vector, as array but can change size
    * (Double-linked) list
    * Hashtable

**Impossible**

* Nested assoc arrays
* Read/write from PHP session
* Array init in both C and PHP
** BUT: Maybe possible with `#__C__` and sed? Or just sed?
* UTF8

**Snippets**

To include PHP which is ignored in C:

```php
// ?> <?php $arr = $arr[0]; ?>
```

## Use-cases

* Read database config from JSON file
* Read/write from/to database
* Curl call somewhere
* Some mathematical thing (n-body problem?)
* Export something big to CSV
* IRC-bot

## Open questions

* String concat
* Local type inference
* Assoc array is banned, hash table with string keys is ok

## Benchmark

strip binary first

    $ time f r a in {1..100}; do php polyglot.php; done
    $ time for a in {1..100}; do ./polyglot; done

    C version:
    real    0m0,532s
    user    0m0,266s
    sys     0m0,217s

    PHP version:
    real    0m2,211s
    user    0m1,338s
    sys     0m0,810s

## Transpiler

* What type information is needed?

```php
function add(int $x, int $t): int {
    return $x + $y;
}

function add_points(Point $p1, Point $p2) {
    return new Point($x->x
}
```

setVariableAndTokenMappingsForExpressionManager

## Notes

Topic is language design and type-inference.

PHP has a weird "array" type that's basically a hash table that also acts as a list. When transpiling PHP to a more machine-close language, one optimization is to infer the "array" type to a more precise type, e.g. to an actual C-like array, linked list, dynamic vector, or a traditional hash table.

Some example code:

```php
$arr = [1, 2, 3];     // Fixed size integer array
/** @var int[42] */
$arr = [];            // Fixed-size int array with size 42
$arr[] = 4;           // Pushing a new element on top of $arr, so either dynamic vector or linked list
$arr["moo"] = 5;      // Now it's a hash table
$tuple = [1, "moo"];  // This could be a tuple, since it has different types
```

The challenge is to deal with these different alternatives when only limited information is available.

```php
function foo(array $arr): string {
    return $arr[0];  // Assume a string array, that is, string[]
}

function foo(int $a, string $b): array {
    return [$a, $b];  // Tuple with size 2
}
```

Possible to enforce stack alloc for class? So can't combine with heap alloc classes.

```php
/** @pholyglot stack-only */
class Point
{
}
```

Arrays

```php
use _ as tuple;

/**
 * @param array<int>  // Fixed or dynamic or list?
 */
function foo(array $arr): void {
}

$foo = /** @alloc stack */ new Foo();
$foo = /** @alloc pool */ new Foo();

/**
 * @param-pholly vect<int>
 * @return-pholly tuple
 */
function bar(array &$a): array
{
    $b = $a[0];  // Fixed-size
    return [$b];
}
```

```
/**
 * OK in Psalm
 * @return &array<int>
 */
function &foo(): array
{
    $a = [1, 2, 3];
    return $a;
}
[$a, $b, $c] = &foo();
echo $a;
echo $b;
echo $c;
```

Error. All good if error message is empty (as in Go).

```
function bar(): array
{
    //return [128, null];
    return [127, "error message"];
}
if (([$result, $error] = bar()) === [$result, null]) {
    echo 'all good, we got result ' . $result;
} else {
    echo $error;
}
```

## gcc

    __cleanup__

    http://echorand.me/site/notes/articles/c_cleanup/cleanup_attribute_c.html

Stack alloc vs value types:

    /**
     * @return Point@stack
     */
    function makePoint() {
        $p = /** @alloc stack */ new Point();
        ...
        return $p;           // Break;
        return new Point();  // Works with value types...?
    }

    /**
     * @param SplDoublyLinkedList $points
     */
    function addPointToList(SplDoublyLinkedList $points) {
        // Pool, Boehm, ref count
        $points->push(new Point());
    }

    // Can be done with both pool, boehm, ref count?
    // Polymorph on alloc kind?
    array_slice($arr, 0, 1);

Pool:

    $foos = /** @mem pool */ [new Foo(), new Foo(), new Foo()];
    $slice = array_slice($foos, 0, 1);
    return $slice;  // Pool can't escape, same as stack?

Arena is not the same as a pool.

Make long == double == uintptr_t same size

    21:46 size_t a = sizeof(long), b = sizeof(double), c = sizeof(uintptr_t); if (a == b && b == c) {}
    22:01 static_assert(sizeof(long) == sizeof(double) == sizeof(uintptr_t));

    $ gcc -E -dM - < /dev/null | egrep 'UINTPTR_TYPE|SIZEOF_LONG'
    #define __SIZEOF_LONG__ 8
    #define __SIZEOF_LONG_DOUBLE__ 16
    #define __UINTPTR_TYPE__ long unsigned int
    #define __SIZEOF_LONG_LONG__ 8
    $ x86_64-w64-mingw32-gcc -E -dM - < /dev/null | egrep 'UINTPTR_TYPE|SIZEOF_LONG'
    #define __SIZEOF_LONG__ 4
    #define __SIZEOF_LONG_DOUBLE__ 16
    #define __UINTPTR_TYPE__ long long unsigned int
    #define __SIZEOF_LONG_LONG__ 8

    x86_64 tolerates unaligned access but it's slow. Other platforms normally just segfaults.
    alignof(max_align_t) is the safest alignment that works for all types.

Single owner as a solution to malloc from mysqli and other external libs?

PHP FFI could be used?
