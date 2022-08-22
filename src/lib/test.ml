open Base

let%test_unit "trivial" =
    let source = "<?php // @pholyglot" in
    let linebuf = Lexing.from_string source in
    let ast = Parser.program Lexer.token linebuf in
    [%test_eq: Ast.program] ast (Declaration_list [])

let%test_unit "trivial main" =
    let source = "<?php // @pholyglot
    function main(): int {
        return 0;
    }
    " in
    let linebuf = Lexing.from_string source in
    let ast = Parser.program Lexer.token linebuf in
    [%test_eq: Ast.program] ast (Declaration_list [
        Function ("main", [], [
            Return (Num 0)
        ], Int)
    ])

let%test_unit "trivial assignment" =
    let source = "<?php // @pholyglot
    function main(): int {
        $a = 0;
        return $a;
    }
    " in
    let linebuf = Lexing.from_string source in
    let ast = Parser.program Lexer.token linebuf in
    [%test_eq: Ast.program] ast (Declaration_list [
        Function ("main", [], [
            Assignment (Infer_me, "a", Num 0);
            Return (Variable "a")
        ], Int)
    ])

let%test_unit "trivial arith" =
    let source = "<?php // @pholyglot
    function main(): int {
        $a = 0;
        return $a + 1 - 1 * 1 / 1;
    }
    " in
    let linebuf = Lexing.from_string source in
    let ast = Parser.program Lexer.token linebuf in
    [%test_eq: Ast.program] ast (Declaration_list [
        Function ("main", [], [
            Assignment (Infer_me, "a", Num 0);
            Return (Minus (Plus (Variable "a", Num 1), (Div (Times (Num 1, Num 1), (Num 1)))))
        ], Int)
    ])

(* Transpile from Pholly AST to polyglot AST *)
let%test_unit "trivial transpile" =
    let ast = Ast.Declaration_list [
        Function ("main", [], [
            Return (Num 0)
        ], Int)
    ] in
    let phast = Transpile.run ast in
    let pholyglot_code = Pholyglot_ast.string_of_program phast in
    [%test_eq: string] pholyglot_code {|//<?php echo "\x08\x08"; ob_start(); ?>
#include <stdio.h>
#include <glib.h>
#define function 
#define __PHP__ 0
#if __PHP__//<?php
class GString { public $str; public function __construct($str) { $this->str = $str; } }
function g_string_new(string $str) { return new GString($str); }
function g_string_append(GString $s1, string $s2) { return new GString($s1->str . $s2); }
#endif//?>
//<?php
#__C__ int
function main()
{
    return 0;
}
// ?>
// <?php ob_end_clean(); main();|}

let%test_unit "trivial arith transpile" =
    let ast  = Ast.Declaration_list [
        Function ("main", [], [
            Assignment (Infer_me, "a", Num 0);
            Return (Minus (Plus (Variable "a", Num 1), (Div (Times (Num 1, Num 1), (Num 1)))))
        ], Int)
    ] in
    let ast = Infer.run ast in
    let phast = Transpile.run ast in
    let pholyglot_code = Pholyglot_ast.string_of_program phast in
    [%test_eq: string] pholyglot_code {|//<?php echo "\x08\x08"; ob_start(); ?>
#include <stdio.h>
#include <glib.h>
#define function 
#define __PHP__ 0
#if __PHP__//<?php
class GString { public $str; public function __construct($str) { $this->str = $str; } }
function g_string_new(string $str) { return new GString($str); }
function g_string_append(GString $s1, string $s2) { return new GString($s1->str . $s2); }
#endif//?>
//<?php
#__C__ int
function main()
{
    #__C__ int
    $a = 0;
    return $a + 1 - 1 * 1 / 1;
}
// ?>
// <?php ob_end_clean(); main();|}

let%test_unit "trivial string" =
    let source = {|<?php // @pholyglot
    function main(): int {
        $str = "Hello";
        return 0;
    }
    |} in
    let linebuf = Lexing.from_string source in
    let ast = Parser.program Lexer.token linebuf in
    [%test_eq: Ast.program] ast (Declaration_list [
        Function ("main", [], [
            Assignment (Infer_me, "str", String "\"Hello\"");
            Return (Num 0)
        ], Int)
    ])

let%test_unit "trivial concat" =
    let source = {|<?php // @pholyglot
    function main(): int {
        $str = "Hello" . "world";
        return 0;
    }
    |} in
    let linebuf = Lexing.from_string source in
    let ast = Parser.program Lexer.token linebuf in
    [%test_eq: Ast.program] ast (Declaration_list [
        Function ("main", [], [
            Assignment (Infer_me, "str", Concat (String "\"Hello\"", String "\"world\""));
            Return (Num 0)
        ], Int)
    ])

let%test "trivial concat type error" =
    let source = {|<?php // @pholyglot
    function main(): int {
        $str = "Hello" . 1;
        return 0;
    }
    |} in
    let linebuf = Lexing.from_string source in
    let ast = Parser.program Lexer.token linebuf in
    try 
        ignore (Infer.run ast);
        false
    with
         | Infer.Type_error s -> true
         | _ -> false

let%test_unit "transpile concat" =
    let source = {|<?php // @pholyglot
    function main(): int {
        $str = "Hello" . " world" . "!";
        return 0;
    }
    |} in
    let linebuf = Lexing.from_string source in
    let ast = Parser.program Lexer.token linebuf in
    let ast = Infer.run ast in
    let phast = Transpile.run ast in
    let pholyglot_code = Pholyglot_ast.string_of_program phast in
    [%test_eq: string] pholyglot_code {|//<?php echo "\x08\x08"; ob_start(); ?>
#include <stdio.h>
#include <glib.h>
#define function 
#define __PHP__ 0
#if __PHP__//<?php
class GString { public $str; public function __construct($str) { $this->str = $str; } }
function g_string_new(string $str) { return new GString($str); }
function g_string_append(GString $s1, string $s2) { return new GString($s1->str . $s2); }
#endif//?>
//<?php
#__C__ int
function main()
{
    #__C__ GString*
    $str = g_string_append(g_string_append(g_string_new("Hello"), g_string_new(" world")->str), g_string_new("!")->str);
    return 0;
}
// ?>
// <?php ob_end_clean(); main();|}

(* TODO: *)
(* $b = [1, 2, 3]; *)
(* function foo(mixed $a): array *)
(* Array value semantics - must use '&'? *)
