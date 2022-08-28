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
            Assignment (Infer_me, Variable "a", Num 0);
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
            Assignment (Infer_me, Variable "a", Num 0);
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
            Assignment (Infer_me, Variable "a", Num 0);
            Return (Minus (Plus (Variable "a", Num 1), (Div (Times (Num 1, Num 1), (Num 1)))))
        ], Int)
    ] in
    let ast = Infer.run (Namespace.create ()) ast in
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
    $a 
    = 0;
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
            Assignment (Infer_me, Variable "str", String "\"Hello\"");
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
            Assignment (Infer_me, Variable "str", Concat (String "\"Hello\"", String "\"world\""));
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
        ignore (Infer.run (Namespace.create ()) ast);
        false
    with
         | Infer.Type_error _ -> true
         | _ -> false

let%test_unit "trivial array" =
    let source = {|<?php // @pholyglot
    function main(): int {
        $arr = [1, 2, 3];
        return 0;
    }
    |} in
    let linebuf = Lexing.from_string source in
    let ast = Parser.program Lexer.token linebuf in
    [%test_eq: Ast.program] ast (Declaration_list [
        Function ("main", [], [
            Assignment (Infer_me, Variable "arr", Array_init ([Num 1; Num 2; Num 3]));
            Return (Num 0)
        ], Int)
    ])

let%test_unit "trivial array infer" =
    let source = {|<?php // @pholyglot
    function main(): int {
        $arr = [1, 2, 3];
        printf("%d", $arr[0]);
        return 0;
    }
    |} in
    let linebuf = Lexing.from_string source in
    let ns = Namespace.create () in
    let ast = Parser.program Lexer.token linebuf |> Infer.run ns in
    [%test_eq: Ast.program] ast (Declaration_list [
        Function ("main", [], [
            Assignment (Fixed_array (Int, 3), Variable "arr", Array_init ([Num 1; Num 2; Num 3]));
            Function_call (Function_type (Void, [String_literal; Int]), "printf", [Coerce (String_literal, String "\"%d\""); Array_access ("arr", Num 0)]);
            Return (Num 0)
        ], Int)
    ])

let%test_unit "trivial array infer and print" =
    let ns = Namespace.create () in
    let ast = Ast.Declaration_list [
        Function ("main", [], [
            Assignment (Fixed_array (Int, 3), Variable "arr", Array_init ([Num 1; Num 2; Num 3]));
            Function_call (Infer_me, "printf", [String "\"%d\""; Array_access ("arr", Num 0)]);
            Return (Num 0)
        ], Int)
    ] |> Infer.run ns in
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
    $arr 
    #__C__ [3]
    = 
#__C__ {
#if __PHP__
[
#endif
    1, 2, 3
#if __PHP__
]
#endif
#__C__ }
    ;
     printf("%d", $arr[0]);
    return 0;
}
// ?>
// <?php ob_end_clean(); main();|}

let%test_unit "transpile concat" =
    let source = {|<?php // @pholyglot
    function main(): int {
        $str = "Hello" . " world" . "!";
        return 0;
    }
    |} in
    let linebuf = Lexing.from_string source in
    let ast = Parser.program Lexer.token linebuf in
    let ns = Namespace.create () in
    let ast = Infer.run ns ast in
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
    $str 
    = g_string_append(g_string_append(g_string_new("Hello"), g_string_new(" world")->str), g_string_new("!")->str);
    return 0;
}
// ?>
// <?php ob_end_clean(); main();|}

let%test_unit "trivial escape" =
    (* TODO: Int is allowed to escape since it's copied *)
    let ast = Ast.Declaration_list [
        Function ("main", [], [
            Assignment (Infer_me, Variable "a", Num 0);
            Return (Variable "a")
        ], Int)
    ] in
    let escape_status = Escape.run ast in
    ()

let%test_unit "infer_printf 1" =
    let t = Infer.infer_printf "%d" in
    [%test_eq: Ast.typ list] t [Ast.Int]

let%test_unit "infer_printf 2" =
    let t = Infer.infer_printf "%s" in
    [%test_eq: Ast.typ list] t [Ast.String_literal]

let%test_unit "infer_printf 3" =
    let t = Infer.infer_printf "%s %d" in
    [%test_eq: Ast.typ list] t [Ast.String_literal; Ast.Int]

let%test_unit "infer_printf 4" =
    let t = Infer.infer_printf "Bla bla $something !!! %s moo foo %deee" in
    [%test_eq: Ast.typ list] t [Ast.String_literal; Ast.Int]

let%test_unit "double printf" =
    let source = {|<?php // @pholyglot
    function main(): int {
        printf("%s %d", "Hello", 1);
        return 0;
    }
    |} in
    let linebuf = Lexing.from_string source in
    let ns = Namespace.create () in
    let ast = Parser.program Lexer.token linebuf |> Infer.run ns in
    (*
    let phast = Transpile.run ast in
    let pholyglot_code = Pholyglot_ast.string_of_program phast in
    [%test_eq: string] pholyglot_code ""
    *)

    [%test_eq: Ast.program] ast (Declaration_list [
        Function ("main", [], [
            Function_call (
                Function_type (Void, [String_literal; String_literal; Int]),
                "printf",
                [
                    Coerce (String_literal, String "\"%s %d\"");
                    Coerce (String_literal, String "\"Hello\"");
                    Num 1
                ]
            );
            Return (Num 0)
        ], Int)
    ])

let%test_unit "trivial class declare" =
    let source = {|<?php // @pholyglot
    class Point {
        public int $x;
        public int $y;
    }
    function main(): int {
        return 0;
    }
    |} in
    let linebuf = Lexing.from_string source in
    let ast = Parser.program Lexer.token linebuf in
    [%test_eq: Ast.program] ast (Declaration_list [
        Class ("Point", [
            ("x", Int);
            ("y", Int)
        ]);
        Function ("main", [], [
            Return (Num 0)
        ], Int)
    ])

let%test_unit "class new" =
    let source = {|<?php // @pholyglot
    class Point {
        public int $x;
        public int $y;
    }
    function main(): int {
        $p = new Point();
        return 0;
    }
    |} in
    let linebuf = Lexing.from_string source in
    let ast = Parser.program Lexer.token linebuf in
    [%test_eq: Ast.program] ast (Declaration_list [
        Class ("Point", [
            ("x", Int);
            ("y", Int)
        ]);
        Function ("main", [], [
            Assignment (Infer_me, Variable "p", (New (Class_type "Point", [])));
            Return (Num 0)
        ], Int)
    ])

let%test_unit "object lvalue assignment" =
    let source = {|<?php // @pholyglot
    class Point {
        public int $x;
        public int $y;
    }
    function main(): int {
        $p = new Point();
        $p->x = 1;
        return 0;
    }
    |} in
    let linebuf = Lexing.from_string source in
    let ast = Parser.program Lexer.token linebuf in
    [%test_eq: Ast.program] ast (Declaration_list [
        Class ("Point", [
            ("x", Int);
            ("y", Int)
        ]);
        Function ("main", [], [
            Assignment (Infer_me, Variable "p", (New (Class_type "Point", [])));
            Assignment (Infer_me, Object_access ("p", Property_access "x"), (Num 1));
            Return (Num 0)
        ], Int)
    ])

let%test_unit "object object access in expression" =
    let source = {|<?php // @pholyglot
    class Point {
        public int $x;
        public int $y;
    }
    function main(): int {
        $p = new Point();
        $p->x = 1;
        printf("%d", $p->x);
        return 0;
    }
    |} in
    let linebuf = Lexing.from_string source in
    let ast = Parser.program Lexer.token linebuf in
    [%test_eq: Ast.program] ast (Declaration_list [
        Class ("Point", [
            ("x", Int);
            ("y", Int)
        ]);
        Function ("main", [], [
            Assignment (Infer_me, Variable "p", (New (Class_type "Point", [])));
            Assignment (Infer_me, Object_access ("p", Property_access "x"), (Num 1));
            Function_call (Infer_me, "printf", [String "\"%d\""; Object_access ("p", Property_access "x")]);
            Return (Num 0)
        ], Int)
    ])

let%test_unit "infer object access" =
    let ns = Namespace.create () in
    let ast : Ast.program = Declaration_list [
        Class ("Point", [
            ("x", Int);
            ("y", Int)
        ]);
        Function ("main", [], [
            Assignment (Infer_me, Variable "p", (New (Class_type "Point", [])));
            Assignment (Infer_me, Object_access ("p", Property_access "x"), (Num 1));
            Function_call (Infer_me, "printf", [String "\"%d\""; Object_access ("p", Property_access "x")]);
            Return (Num 0)
        ], Int)
    ] |> Infer.run ns in
    [%test_eq: Ast.program] ast (Declaration_list [
        Class ("Point", [
            ("x", Int);
            ("y", Int)
        ]);
        Function ("main", [], [
            Assignment (Class_type "Point", Variable "p", (New (Class_type "Point", [])));
            Assignment (Infer_me, Object_access ("p", Property_access "x"), (Num 1));
            Function_call (Infer_me, "printf", [String "\"%d\""; Object_access ("p", Property_access "x")]);
            Return (Num 0)
        ], Int)
    ])

(* TODO: *)
(* $b = [1, 2, 3];  Vector, array, linked list? SPL *)
(* $b = [];  Empty list, expect error *)
(* $b = [1, "Moo"];  Tuple *)
(* return [1, 2, 3]; *)
(* Array value semantics - must use '&'? *)
(* $arr["moo"] = 4 Hash table *)
(* foreach ([1, 2, 3] as $i) *)
(* function foo(mixed $a): array *)
(* new Foo() *)
(* MySQL *)
(* Curl *)
(* ini file *)
(* $a = 1 + 2.0; int vs float *)
(* enum? *)
(* array_map([1, 2, 3], fn (x) => x + 1); *)
(* Nullable types *)
(* instanceof to refine types, requires runtime type info *)
(* $a = 0;  return $a; // $a escapes *)
(* Lambda, or anonym function inside function scope $fn = fn (x) => "moo"; $fn(123); *)
(* printf("%s %d %i") etc, format processing *)
(* class Foo { public $moo; } *)
(* class Foo { private $moo; } *)
(* class Foo { private $moo; public function hey() {} } *)
(* class Foo extends Bar; *)
(* Class name must start with capital letter *)
(* Escape analysis of returning linked list *)
(* Use escape analysis to free strings that do not escape, by injecting free() before return. *)
(* function foo(): int { return "moo"; } Invalid return type *)
(* Different alloc types: heap, stack, pool, depending on escape status *)
