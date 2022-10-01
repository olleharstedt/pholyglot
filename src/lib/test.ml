open Printf
open Base
open Expect_test_helpers_base
module Log = Dolog.Log

let rec string_of_token (token : Parser.token) : string =
    let open Parser in
    match token with
        | STRING_LITERAL s -> "STRING_LITERAL " ^ s
        | SEMICOLON -> "SEMICOLON"
        | RPAREN -> "RPAREN"
        | RETURN -> "RETURN"
        | RBRACK -> "RBRACE"
        | RBRACE -> "RBRACE"
        | PLUS -> "PLUS"
        | NEW -> "NEW"
        | NAME s  -> "NAME " ^ s
        | CLASS_NAME s -> "CLASS_NAME " ^ s
        | MINUS -> "MINUS"
        | LT -> "LT"
        | GT -> "GT"
        | LPAREN -> "LPAREN"
        | LBRACK -> "LBRACK"
        | LBRACE -> "LBRACE"
        | INT i -> "INT" ^ string_of_int i
        | EQEQ -> "EQEQ"
        | EQ -> "EQ"
        | EOF -> "EOF"
        | START_SCRIPT -> "START_SCRIPT"
        | FUNCTION -> "FUNCTION"
        | DOT -> "DOT"
        | COLON -> "COLON"
        | DOLLAR -> "DOLLAR"
        | QUOTE -> "QUOTE"
        | DOCBLOCK_PARAM -> "DOCBLOCK_PARAM"
        | VOID_TYPE -> "VOID_TYPE"
        | ARRAY_TYPE -> "ARRAY_TYPE"
        | AMPERSAND -> "AMPERSAND"
        | TIMES -> "TIMES"
        | DIV -> "DIV"
        | COMMA -> "COMMA"
        | ARROW -> "ARROW"
        | INT_TYPE -> "INT_TYPE"
        | STRING_TYPE -> "STRING_TYPE"
        | CLASS -> "CLASS"
        | PUBLIC -> "PUBLIC"
        | DOCBLOCK l -> sprintf "DOCBLOCK (%s)" (List.fold_left l ~init:"" ~f:(fun a b -> a ^ " " ^ string_of_token b))
        | _ -> failwith "string_of_token: Unknown token"

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
        Function {
            name = "main";
            docblock = [];
            params = [];
            stmts = [
                Return (Num 0)
            ];
            function_type = Function_type {return_type = Int; arguments = []}
        }
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
    [%test_eq: Ast.program] ast (Ast.Declaration_list [
        Function {
            name= "main";
            docblock = [];
            params = [];
            stmts = [
                Assignment (Infer_me, Variable "a", Num 0);
                Return (Variable "a");
            ];
            function_type = Function_type {return_type = Int; arguments = []}
        }
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
        Function {
            name = "main";
            docblock = [];
            params = [];
            stmts = [
                Assignment (Infer_me, Variable "a", Num 0);
                Return (Minus (Plus (Variable "a", Num 1), (Div (Times (Num 1, Num 1), (Num 1)))))
            ];
            function_type = Function_type {return_type = Int; arguments = []}
        }
    ])

(* Transpile from Pholly AST to polyglot AST *)
let%test_unit "trivial transpile" =
    let ast = Ast.Declaration_list [
        Function {
            name = "main";
            docblock = [];
            params = [];
            stmts = [
                Return (Num 0)
            ];
            function_type = Function_type {return_type = Int; arguments = []}
        }
    ] in
    let phast = Transpile.run ast in
    let pholyglot_code = Pholyglot_ast.string_of_program phast in
    [%test_eq: string] pholyglot_code {|//<?php echo "\x08\x08"; ob_start(); ?>
#include <stdio.h>
#include <glib.h>
#define $ 
#define class struct
#define __PHP__ 0
#define new_(x) alloca(sizeof(struct x))
#if __PHP__//<?php
class GString { public $str; public function __construct($str) { $this->str = $str; } }
function g_string_new(string $str) { return new GString($str); }
function g_string_append(GString $s1, string $s2) { return new GString($s1->str . $s2); }
function new_($class) { return new $class; }
#endif//?>
//<?php
#define function int
function main()
{
    return 0;
}
#undef function
// ?>
// <?php ob_end_clean(); main();|}

let%test_unit "trivial arith transpile" =
    let ast  = Ast.Declaration_list [
        Function {
            name = "main";
            docblock = [];
            params = [];
            stmts = [
                Assignment (Infer_me, Variable "a", Num 0);
                Return (Minus (Plus (Variable "a", Num 1), (Div (Times (Num 1, Num 1), (Num 1)))))
            ];
            function_type = Function_type {return_type = Int; arguments = []}}
    ] in
    let ast = Infer.run (Namespace.create ()) ast in
    let phast = Transpile.run ast in
    let pholyglot_code = Pholyglot_ast.string_of_program phast in
    [%test_eq: string] pholyglot_code {|//<?php echo "\x08\x08"; ob_start(); ?>
#include <stdio.h>
#include <glib.h>
#define $ 
#define class struct
#define __PHP__ 0
#define new_(x) alloca(sizeof(struct x))
#if __PHP__//<?php
class GString { public $str; public function __construct($str) { $this->str = $str; } }
function g_string_new(string $str) { return new GString($str); }
function g_string_append(GString $s1, string $s2) { return new GString($s1->str . $s2); }
function new_($class) { return new $class; }
#endif//?>
//<?php
#define function int
function main()
{
    #__C__ int
    $a 
    = 0;
    return $a + 1 - 1 * 1 / 1;
}
#undef function
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
        Function {
            name = "main";
            docblock = [];
            params = [];
            stmts = [
                Assignment (Infer_me, Variable "str", String "\"Hello\"");
                Return (Num 0);
            ];
            function_type = Function_type {return_type = Int; arguments = []}
        }
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
        Function {
            name = "main";
            docblock = [];
            params = [];
            stmts = [
                Assignment (Infer_me, Variable "str", Concat (String "\"Hello\"", String "\"world\""));
                Return (Num 0)
            ];
            function_type = Function_type {return_type = Int; arguments = []}
        }
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
        Function {
            name = "main";
            docblock = [];
            params = [];
            stmts = [
                Assignment (Infer_me, Variable "arr", Array_init ([Num 1; Num 2; Num 3]));
                Return (Num 0);
            ];
            function_type = Function_type {return_type = Int; arguments = []}
        }
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
        Function {
            name = "main";
            docblock = [];
            params = [];
            stmts = [
                Assignment (Fixed_array (Int, Some 3), Variable "arr", Array_init ([Num 1; Num 2; Num 3]));
                Function_call (
                    Function_type {return_type = Void; arguments = [String_literal; Int]},
                    "printf",
                    [Coerce (String_literal, String "\"%d\""); Array_access ("arr", Num 0)]
                );
                Return (Num 0);
            ];
            function_type = Function_type {return_type = Int; arguments = []}
        }
    ])

let%test_unit "trivial array infer and print" =
    Log.set_log_level Log.DEBUG;
    (* Location becomes ./_build/default/lib/debug.txt *)
    Log.set_output (open_out "debug.txt");
    Log.debug "trivial arith infer and print";
    let ns = Namespace.create () in
    let ast = Ast.Declaration_list [
        Function {
            name = "main";
            docblock = [];
            params = [];
            stmts = [
                Assignment (Infer_me, Variable "arr", Array_init ([Num 1; Num 2; Num 3]));
                Function_call (Infer_me, "printf", [String "\"%d\""; Array_access ("arr", Num 0)]);
                Return (Num 0);
            ];
            function_type = Function_type {return_type = Int; arguments = []}
        }
    ] |> Infer.run ns in
    Log.set_log_level Log.FATAL;
    Log.clear_prefix ();
    Log.debug "should not be visible";
    let phast = Transpile.run ast in
    let pholyglot_code = Pholyglot_ast.string_of_program phast in
    [%test_eq: string] pholyglot_code {|//<?php echo "\x08\x08"; ob_start(); ?>
#include <stdio.h>
#include <glib.h>
#define $ 
#define class struct
#define __PHP__ 0
#define new_(x) alloca(sizeof(struct x))
#if __PHP__//<?php
class GString { public $str; public function __construct($str) { $this->str = $str; } }
function g_string_new(string $str) { return new GString($str); }
function g_string_append(GString $s1, string $s2) { return new GString($s1->str . $s2); }
function new_($class) { return new $class; }
#endif//?>
//<?php
#define function int
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
#undef function
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
#define $ 
#define class struct
#define __PHP__ 0
#define new_(x) alloca(sizeof(struct x))
#if __PHP__//<?php
class GString { public $str; public function __construct($str) { $this->str = $str; } }
function g_string_new(string $str) { return new GString($str); }
function g_string_append(GString $s1, string $s2) { return new GString($s1->str . $s2); }
function new_($class) { return new $class; }
#endif//?>
//<?php
#define function int
function main()
{
    #__C__ GString*
    $str 
    = g_string_append(g_string_append(g_string_new("Hello"), g_string_new(" world")->str), g_string_new("!")->str);
    return 0;
}
#undef function
// ?>
// <?php ob_end_clean(); main();|}

let%test_unit "two functions ast" =
    let source = {|<?php // @pholyglot
function foo(int $c): int {
    return $c + 20;
}
function main(): int {
    $b = foo(10 + 20);
    return $b + 30;
}
|} in
    let linebuf = Lexing.from_string source in
    let ns = Namespace.create () in
    let ast = Parser.program Lexer.token linebuf |> Infer.run ns in
    [%test_eq: Ast.program] ast (Ast.Declaration_list [
        Function {
            name = "foo";
            docblock = [];
            params = [Param ("c", Int)];
            stmts = [Return (Plus ((Variable "c"), (Num 20)))];
            function_type = Function_type {return_type = Int; arguments = [Int]}
        };
        Function { 
            name = "main";
            docblock = [];
            params = [];
            stmts = [
                Assignment (Int, Variable "b", Function_call (Function_type {return_type = Int; arguments = [Int]}, "foo", [Plus ((Num 10), (Num 20))]));
                Return (Plus (Variable "b", Num 30))
            ];
            function_type = Function_type {return_type = Int; arguments = []}
        }
    ])

let%test_unit "two functions to pholyglot" =
    let code = [
        Pholyglot_ast.Function (
            "foo",
            [Param ("c", Int)],
            [Return (Plus ((Variable "c"), (Num 20)))],
            Function_type {return_type = Int; arguments = [Int]}
        );
        Pholyglot_ast.Function (
            "main",
            [],
            [
                Assignment (Int, Variable "b", Function_call (Function_type {return_type = Int; arguments = [Int]}, "foo", [Plus ((Num 10), (Num 20))]));
                Return (Plus (Variable "b", Num 30))
            ],
            Function_type {return_type = Int; arguments = []}
        )
    ] |> Pholyglot_ast.string_of_declares
    in
    [%test_eq: string] code {|#define function int
function foo(int $c)
{
    return $c + 20;
}
#undef function
#define function int
function main()
{
    #__C__ int
    $b 
    = foo(10 + 20);
    return $b + 30;
}
#undef function
|}

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
    [%test_eq: Ast.program] ast (Declaration_list [
        Function {
            name = "main";
            docblock = [];
            params = [];
            stmts = [
                Function_call (
                    Function_type {return_type = Void; arguments = [String_literal; String_literal; Int]},
                    "printf",
                    [
                        Coerce (String_literal, String "\"%s %d\"");
                        Coerce (String_literal, String "\"Hello\"");
                        Num 1
                    ]
                );
                Return (Num 0);
            ];
            function_type = Function_type {return_type = Int; arguments = []}
        }
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
        Class ("Point", Infer_kind, [
            ("__object_property_x", Int);
            ("__object_property_y", Int)
        ]);
        Function {
            name = "main";
            docblock = [];
            params = [];
            stmts = [
                Return (Num 0);
            ];
            function_type = Function_type {return_type = Int; arguments = []}
        }
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
        Class ("Point", Infer_kind, [
            ("__object_property_x", Int);
            ("__object_property_y", Int)
        ]);
        Function {
            name = "main";
            docblock = [];
            params = [];
            stmts = [
                Assignment (Infer_me, Variable "p", (New (Class_type ("Point"), [])));
                Return (Num 0)
            ];
            function_type = Function_type {return_type = Int; arguments = []}
        }
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
        Class ("Point", Infer_kind, [
            ("__object_property_x", Int);
            ("__object_property_y", Int)
        ]);
        Function {
            name = "main";
            docblock = [];
            params = [];
            stmts = [
                Assignment (Infer_me, Variable "p", (New (Class_type ("Point"), [])));
                Assignment (Infer_me, Object_access ("p", Property_access "__object_property_x"), (Num 1));
                Return (Num 0);
            ];
            function_type = Function_type {return_type = Int; arguments = []}
        }
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
        Class ("Point", Infer_kind, [
            ("__object_property_x", Int);
            ("__object_property_y", Int)
        ]);
        Function {
            name = "main";
            docblock = [];
            params = [];
            stmts = [
                Assignment (Infer_me, Variable "p", (New (Class_type ("Point"), [])));
                Assignment (Infer_me, Object_access ("p", Property_access "__object_property_x"), (Num 1));
                Function_call (Infer_me, "printf", [String "\"%d\""; Object_access ("p", Property_access "__object_property_x")]);
                Return (Num 0);
            ];
            function_type = Function_type {return_type = Int; arguments = []}
        }
    ])

let%test_unit "infer object access" =
    let ns = Namespace.create () in
    let ast : Ast.program = Ast.Declaration_list [
        Class ("Point", Infer_kind, [
            ("x", Int);
            ("y", Int)
        ]);
        Function {
            name = "main";
            docblock = [];
            params = [];
            stmts = [
                Assignment (Infer_me, Variable "p", (New (Class_type ("Point"), [])));
                Assignment (Infer_me, Object_access ("p", Property_access "x"), (Num 1));
                Function_call (Infer_me, "printf", [String "\"%d\""; Object_access ("p", Property_access "x")]);
                Return (Num 0);
            ];
            function_type = Function_type {return_type = Int; arguments = []}
        }
    ] |> Infer.run ns in
    [%test_eq: Ast.program] ast (Declaration_list [
        Class ("Point", Val, [
            ("x", Int);
            ("y", Int)
        ]);
        Function {
            name = "main";
            docblock = [];
            params = [];
            stmts = [
                Assignment (Class_type "Point", Variable "p", (New (Class_type ("Point"), [])));
                Assignment (Int, Object_access ("p", Property_access "x"), (Num 1));
                Function_call (
                    Function_type {return_type = Void; arguments = [String_literal; Int]},
                    "printf",
                    [Coerce (String_literal, String "\"%d\""); Object_access ("p", Property_access "x")]
                );
                Return (Num 0)
            ];
            function_type = Function_type {return_type = Int; arguments = []}
        }
    ])

let%test_unit "output object access" =
    let code = Ast.Declaration_list [
        Class ("Point", Val, [
            ("__object_property_x", Int);
            ("__object_property_y", Int)
        ]);
        Function {
            name = "main";
            docblock = [];
            params = [];
            stmts = [
                Assignment (Class_type "Point", Variable "p", (New (Class_type "Point", [])));
                Assignment (Int, Object_access ("p", Property_access "__object_property_x"), (Num 1));
                Function_call (
                    Function_type {return_type = Void; arguments = [String_literal; Int]},
                    "printf",
                    [
                        Coerce (String_literal, String "\"%d\"");
                        Object_access ("p", Property_access "__object_property_x")
                    ]
                );
                Return (Num 0)
            ];
            function_type = Function_type {return_type = Int; arguments = []}
        }
    ]
         |> Transpile.run
         |> Pholyglot_ast.string_of_program
    in
    [%test_eq: string] code {|//<?php echo "\x08\x08"; ob_start(); ?>
#include <stdio.h>
#include <glib.h>
#define $ 
#define class struct
#define __PHP__ 0
#define new_(x) alloca(sizeof(struct x))
#if __PHP__//<?php
class GString { public $str; public function __construct($str) { $this->str = $str; } }
function g_string_new(string $str) { return new GString($str); }
function g_string_append(GString $s1, string $s2) { return new GString($s1->str . $s2); }
function new_($class) { return new $class; }
#endif//?>
//<?php

class Point {
    #define public int
#define __object_property_x $__object_property_x
    public $__object_property_x;
#undef public
#define public int
#define __object_property_y $__object_property_y
    public $__object_property_y;
#undef public

};
#if __PHP__
define("Point", "Point");  // Needed to make new_() work with C macro
#endif
#define function int
function main()
{
    #__C__ struct Point*
    $p 
    = new_(Point);
    $p->__object_property_x 
    = 1;
     printf("%d", $p->__object_property_x);
    return 0;
}
#undef function
// ?>
// <?php ob_end_clean(); main();|}

let%test_unit "string class property" =
    let source = {|<?php // @pholyglot
class Thing {
    public string $name;
}
function foo(Thing $t): void {
    printf("%s", $t->name);
}
|} in
    let linebuf = Lexing.from_string source in
    let ns = Namespace.create () in
    let ast = Parser.program Lexer.token linebuf |> Infer.run ns in
    [%test_eq: Ast.program] ast (Ast.Declaration_list [
        Class ("Thing", Ref, [
            ("__object_property_name", String);
        ]);
        Function { 
            name = "foo";
            docblock = [];
            params = [Param ("t", Class_type "Thing")];
            stmts = [
                Function_call (
                    Function_type {return_type = Void; arguments = [String_literal; String_literal]},
                    "printf",
                    [Coerce (String_literal, String "\"%s\""); Coerce (String_literal, Object_access ("t", Property_access "__object_property_name"))]
                );
            ];
            function_type = Function_type {return_type = Void; arguments = [Class_type "Thing"]}
        };
    ])

let%test_unit "alias check" =
    let source = {|<?php // @pholyglot
    function foo(): int {
        $a = 123;
        $b = $a;
        return $b;
    }
    |} in
    let ns = Namespace.create () in
    let ast =
        Lexing.from_string source |>
        Parser.program Lexer.token |>
        Infer.run ns
    in
    [%test_eq: Ast.program] ast (Declaration_list [
        Function {
            name = "foo";
            docblock = [];
            params = [];
            stmts = [
                Assignment (Int, Variable "a", Num 123);
                Assignment (Int, Variable "b", Variable "a");
                Return (Variable "b");
            ];
            function_type = Function_type {return_type = Int; arguments = []}
        }
    ])

let%test "return ref type is invalid" =
    let source = {|<?php // @pholyglot
class User {
    public string $name;
}
function foo(): User
{
    $p = new User();
    return $p;
}
|} in
    let ns = Namespace.create () in
    let ast = try 
        ignore(Lexing.from_string source |>
        Parser.program Lexer.token |>
        Infer.run ns);
        false
    with
         | Infer.Type_error _ -> true
         | _ -> false
    in ast

let%test "return val type is valid" =
    let source = {|<?php // @pholyglot
class Point {
    public int $x;
}
function foo(): Point
{
    $p = new Point();
    return $p;
}
|} in
    let ns = Namespace.create () in
    (*let res = try *)
    let ast = try 
        ignore(Lexing.from_string source |>
        Parser.program Lexer.token |>
        Infer.run ns);
        true
    with
         | Infer.Type_error _ -> false
         | _ -> true
    in ast

let%test "assign wrong object property" =
    let source = {|<?php // @pholyglot
class Point {
    public int $x;
}
function main(): int
{
    $p = new Point();
    $p->x = "moo";
    return 0;
}
|}
    in
    match
        Lexing.from_string source |>
        Parser.program Lexer.token |>
        Infer.run (Namespace.create ())
    with
    | exception Infer.Type_error _ -> true
    | _ -> false

let%test "wrong printf type" =
    let source = {|<?php // @pholyglot
class Point
{
    public int $x;
}
function main(): int
{
    $p = new Point();
    $p->x = 123;
    printf("%s\n", $p->x);
    return 0;
}
|} in
    match
        Lexing.from_string source |>
        Parser.program Lexer.token |>
        Infer.run (Namespace.create ())
    with
        | exception Infer.Type_error _ -> true
        | _ -> false

let%test "invalid to pass array as non-ref" =
    let source = {|<?php // @pholyglot
function foo(array $a): void
{
}
|} in
    match 
        Lexing.from_string source |>
        Parser.program Lexer.token |>
        Infer.run (Namespace.create ())
    with
    | exception Ast.Parser_exception _ -> true
    | _ -> false

let%test "invalid to pass int as ref" =
    let source = {|<?php // @pholyglot
function foo(int &$a): void
{
}
|} in
    match 
        Lexing.from_string source |>
        Parser.program Lexer.token |>
        Infer.run (Namespace.create ())
    with
    | exception Ast.Parser_exception _ -> true
    | _ -> false

let%test_unit "multiline comment" =
    let source = {|<?php // @pholyglot
/*
Comment
*/
|} in
    let ast =
        Lexing.from_string source |>
        Parser.program Lexer.token |>
        Infer.run (Namespace.create ())
    in
    [%test_eq: Ast.program] ast (Declaration_list [])

let%test_unit "multiline comment inline" =
    let source = "<?php // @pholyglot
    /* mo mo */ function /* mo */ main(): /* mo */ int {
        return /* mo
        mo
        */
        0;
    }
    " in
    let linebuf = Lexing.from_string source in
    let ast = Parser.program Lexer.token linebuf in
    [%test_eq: Ast.program] ast (Declaration_list [
        Function {
            name = "main";
            docblock = [];
            params = [];
            stmts = [
                Return (Num 0)
            ];
            function_type = Function_type {return_type = Int; arguments = []}
        }
    ])

let%test_unit "docblock array" =
    let source = "<?php // @pholyglot
    /**
     * @param array <int> $ints
     */
    function foo(array &$ints): void {
    }
    " in
    let linebuf = Lexing.from_string source in
    let rec dump_tokens linebuf =
        let token = Lexer.token linebuf in
        match token with
            | Parser.EOF -> ()
            | t ->
                printf "%s" ((string_of_token t) ^ " ");
                dump_tokens linebuf
    in
    dump_tokens linebuf;

    (*
    let linebuf = Lexing.from_string source in
    let ast = Parser.program Lexer.token linebuf in
    [%test_eq: Ast.program] ast (Declaration_list [
        Function {
            name = "main";
            docblock = [];
            params = [];
            stmts = [];
            function_type = Function_type {return_type = Int; arguments = [Fixed_array (Int, None)]}
        }
    ])
    *)


    (*
let%test "nbody benchmark" =
    let source = {|<?php // @pholyglot
class Body
{
    public int $x;
    public int $y;
    public int $z;
    public int $vx;
    public int $vy;
    public int $vz;
    public int $mass;
}
|} in
    false
    *)

    (*
let%test_unit "composite val type" =
    let source = {|<?php // @pholyglot
class Point {
    public int $x;
    public int $y;
}
class Rectangle {
    public Point $t;
    public Point $b;
}
function main(): int
{
    $p1 = new Point();
    $p2 = new Point();
    $r = new Rectangle();
    $r->t = 10;
    $r->b = $p2;
    return 0;
}
|} in
    let ns = Namespace.create () in
    (*let res = try *)
    let ast = Lexing.from_string source |>
        Parser.program Lexer.token |>
        Infer.run ns
    in
    [%test_eq: Ast.program] ast (Declaration_list [])
    *)

(* TODO: *)
(* Support for double/float. `+` won't type-cast automatically. *)
(* $b = [1, 2, 3];  Vector, array, linked list? SPL *)
(* $b = [];  Empty list, expect error *)
(* $b = [1, "Moo"];  Tuple *)
(* use ArrayAccess as tuple; or foo(array|tuple $a)? with `use _ as tuple;`. But not good intentionality? Psalm won't understand tuple? Array shape *)
(* use ArrayAccess as vect; *)
(* BUT: most of the array_* methods won't work with ArrayAccess objects. *)
(* BUT: You also want [$success, $message] = foo(); idiom for tuples, which will only work with builtin array type *)
(* return [1, 2, 3]; *)
(* Array value semantics - must use '&'? *)
(* When type-infer array, refine it as much as possible: Start conservative with C array, the degrade when needed during inference *)
(*   Degrade order: C array, dynamic array (glib), linked list?, tuple OR hash table with string keys, or exception *)
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
(* Lambda, or anonym function inside function scope $fn = fn (x) => "moo"; $fn(123); *)
(* printf("%s %d %i") etc, format processing *)
(* class Foo { public $moo; } *)
(* class Foo { private $moo; } *)
(* class Foo { private $moo; public function hey() {} } *)
(* class Foo extends Bar; *)
(* Class name must start with capital letter *)
(* function foo(): int { return "moo"; } Invalid return type *)
(* Only one return statement per function allowed? Must be last statement? Unless void type *)
(* Dynamic string on heap vs fixed string on stack (only works for string literal) *)
(* $foo = /** @stack */ new Foo(); *)
(* //21:42 < fizzie> struct Point *p = (struct Point[]){foo()};  // just to be silly *)
(* Objects have their own memory pool UNLESS they're value objects. Or, ref types can define which memory strategy to use, like Boehm, ref count, or pool with controlled aliasing/escape?  *)
(* Define memory model by interface? Pholly\Memory\RefCount. Or better to inject in constructor, to not lock it in? But then runtime? Or must be possible to infer at compile time? *)
(* Cannot only use void-function as statements (no ignore()?) *)
(* Interaction between val types and ref types? Most coerce val types to ref when added? That is, copy mem to correct box. *)
(* Chaining property access, like $rectangle->point->x *)
(* Nullable *)
(* All props must have default values *)
