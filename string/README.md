
Example of string alloc that can be put on stack, if possibly polyglot-like?

```
function main(): int {
    $a = "foo";
    printf("%s", $a . "bar");
    return 0;
}
```

strcat: https://www.programiz.com/c-programming/library-function/string.h/strcat

    char *strcat(char *destination, const char *source)
