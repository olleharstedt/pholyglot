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

* Class can be struct.
* String can use smart strings from PHP core.
* Array can use same array.
* Hashtable gonna be hard. Must use class for that?
* Nested assoc arrays... Impossible?
* GC with Boehm, no long-running scripts?

## Use-cases

* Contact database and spit out JSON
* CURL call together with database interaction
* Some mathematical thing
