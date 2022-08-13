## IRC

    ,cc int $a = 10; printf("%d", $a); // Now I'm curious
    http://www.iso-9899.info/n1570.html#J.5.2p1 [Specialized identifiers] Characters other than the underscore _, letters, and digits, that are not part of the basic source
                     character set (such as the dollar sign $, or characters in national character sets) may appear in an identifier (6.4.2).
    ,cc int \u0024a = 10; // this, on the other hand, is strictly conforming
    "Before C++11, C++ didn't allow Unicode escapes with a code point under 0xA0, except for 0x24 ($), 0x40 (@), and 0x60 (`)."
    "C still has the same restrictions as C++98."

## Notes

    #define \u0024 $
    #define \u003C ?
    #define \u002F /

    #define EMPTY
    EMPTY#ifdef

    GOTO here;
    here:

    gcc polyglot.c -I/usr/include/mysql -I/usr/include/mysql/mysql -lmysqlclient

* Class can be struct.
* String can use smart strings from PHP core.
* Array can use same array.
* Hashtable gonna be hard. Must use class for that?
* GC with Boehm, no long-running scripts?

**Impossible:**

* Nested assoc arrays
* Array init in both C and PHP

**Snippets:**

To include PHP which is ignored in C:

```php
// ?> <?php $arr = $arr[0]; ?>
```

## Use-cases

* Read database config from JSON file
* Read/write from/to database
* Curl call somewhere
* Some mathematical thing (n-body problem?)

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
