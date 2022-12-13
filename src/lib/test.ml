open Printf
open Base
open Expect_test_helpers_base
module Log = Dolog.Log

let rec string_of_doctoken (token : Docblockparser.token) : string =
    let open Docblockparser in
    match token with
    | ARRAY_TYPE -> "array"
    | DOCBLOCK_PARAM -> "DOCBLOCK_PARAM"
    | INT_TYPE -> "int"
    | LT -> "<"
    | GT -> ">"
    | COMMA -> ","
    | NAME s -> "DOC_NAME " ^ s
    | VAR_NAME s  -> "VAR_NAME " ^ s
    | EOF -> "EOF"
    | START_OF_COMMENT -> "START_OF_COMMENT"
    | END_OF_COMMENT -> "END_OF_COMMENT"
    | _ -> failwith "string_of_doctoken: unknown token"

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
        | VAR_NAME s  -> "VAR_NAME " ^ s
        | CLASS_NAME s -> "CLASS_NAME " ^ s
        | MINUS -> "MINUS"
        | LT -> "LT"
        | GT -> "GT"
        | LPAREN -> "LPAREN"
        | LBRACK -> "LBRACK"
        | LBRACE -> "LBRACE"
        | INT i -> "INT" ^ string_of_int i
        | FLOAT i -> "FLOAT" ^ string_of_float i
        | EQEQ -> "EQEQ"
        | EQ -> "EQ"
        | EOF -> "EOF"
        | START_SCRIPT -> "START_SCRIPT"
        | FUNCTION -> "FUNCTION"
        | DOT -> "DOT"
        | COLON -> "COLON"
        | QUOTE -> "QUOTE"
        | VOID_TYPE -> "VOID_TYPE"
        | ARRAY_TYPE -> "ARRAY_TYPE"
        | AMPERSAND -> "AMPERSAND"
        | TIMES -> "TIMES"
        | DIV -> "DIV"
        | COMMA -> "COMMA"
        | ARROW -> "ARROW"
        | INT_TYPE -> "INT_TYPE"
        | FLOAT_TYPE -> "FLOAT_TYPE"
        | STRING_TYPE -> "STRING_TYPE"
        | CLASS -> "CLASS"
        | PUBLIC -> "PUBLIC"
        (*| DOCBLOCK l -> sprintf "DOCBLOCK (%s)" (List.fold_left l ~init:"" ~f:(fun a b -> a ^ " " ^ string_of_doctoken b))*)
        | DOCBLOCK_AS_STR s -> "DOCBLOCK_AS_STR " ^ s
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
    let ast = Ast.Function {
            name = "main";
            docblock = [];
            params = [];
            stmts = [
                Return (Num 0)
            ];
            function_type = Function_type {return_type = Int; arguments = []}
        }
    in
    let phast = Transpile.declaration_to_pholyglot ast in
    let pholyglot_code = Pholyglot_ast.string_of_declare phast in
    [%test_eq: string] pholyglot_code {|#define function int
function main()
#undef function
{
    return 0;
}
|}

let%test_unit "trivial arith transpile" =
    let fn  = 
        Ast.Function {
            name = "main";
            docblock = [];
            params = [];
            stmts = [
                Assignment (Infer_me, Variable "a", Num 0);
                Return (Minus (Plus (Variable "a", Num 1), (Div (Times (Num 1, Num 1), (Num 1)))))
            ];
            function_type = Function_type {return_type = Int; arguments = []}
    } in
    let ast = Infer.infer_declaration fn (Namespace.create ()) in
    let phast = Transpile.declaration_to_pholyglot ast in
    let pholyglot_code = Pholyglot_ast.string_of_declare phast in
    [%test_eq: string] pholyglot_code {|#define function int
function main()
#undef function
{
    //?>
    int
    //<?php
    $a 
    = 0;
    return $a + 1 - 1 * 1 / 1;
}
|}

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
                Assignment (Infer_me, Variable "arr", Array_init (Infer_me, None, [Num 1; Num 2; Num 3]));
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
                Assignment (
                    Fixed_array (Int, Some 3),
                    Variable "arr",
                    Function_call (
                        Function_type {return_type = Fixed_array (Int, Some 3); arguments = [Constant; Int; Int; Int; Int]},
                        "array_make",
                        [Constant "int"; Num 3; Num 1; Num 2; Num 3;]
                    )
                );
                Function_call (
                    Function_type {return_type = Void; arguments = [String_literal; Int]},
                    "printf",
                    [
                        Coerce (String_literal, String "\"%d\"");
                        Function_call (
                            Function_type {return_type = Int; arguments = [Constant; Dynamic_array Int; Int]},
                            "array_get",
                            [Constant "int"; Variable "arr"; Num 0]
                        );
                    ]
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
    let fn = Ast.Function {
            name = "main";
            docblock = [];
            params = [];
            stmts = [
                Assignment (Infer_me, Variable "arr", Array_init (Infer_me, None, [Num 1; Num 2; Num 3]));
                Function_call (Infer_me, "printf", [String "\"%d\""; Array_access ("arr", Num 0)]);
                Return (Num 0);
            ];
            function_type = Function_type {return_type = Int; arguments = []}
        }
    in
    Log.set_log_level Log.FATAL;
    Log.clear_prefix ();
    Log.debug "should not be visible";
    let fn = Infer.infer_declaration fn (Namespace.create ()) in
    let phast = Transpile.declaration_to_pholyglot fn in
    let pholyglot_code = Pholyglot_ast.string_of_declare phast in
    [%test_eq: string] pholyglot_code {|#define function int
function main()
#undef function
{
    //?>
    array
    //<?php
    $arr 
    = array_make(int, 3, 1, 2, 3);
     printf("%d", array_get(int, $arr, 0));
    return 0;
}
|}

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
#define class struct
#define __PHP__ 0
#define new(x) x ## __constructor(alloca(sizeof(struct x)))
#define array(...) {__VA_ARGS__}
#define array_make(type, i, ...) {.thing = (type[]) array(__VA_ARGS__), .length = i}
#define array_get(type, arr, i) ((type*) arr.thing)[i]
#define count(x) x.length
#define pprintf printf
typedef struct array array;
struct array { void* thing; size_t length; };
#if __PHP__//<?php
class GString { public $str; public function __construct($str) { $this->str = $str; } }
function g_string_new(string $str) { return new GString($str); }
function g_string_append(GString $s1, string $s2) { return new GString($s1->str . $s2); }
define("int", "int");
define("float", "float");
define("string", "string");
function array_get($type, $arr, $i) { return $arr[$i]; }
function array_make($type, $length, ...$values) { return $values; }
function pprintf($format, ...$args) { fwrite( STDOUT, sprintf( $format, ...$args)); }
#endif//?>
//<?php
#define function int
function main()
#undef function
{
    //?>
    GString*
    //<?php
    $str 
    = g_string_append(g_string_append(g_string_new("Hello"), g_string_new(" world")->str), g_string_new("!")->str);
    return 0;
}
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
        Pholyglot_ast.Function {
            name = "foo";
            params = [Param ("c", Int)];
            stmts = [Return (Plus ((Variable "c"), (Num 20)))];
            function_type = Function_type {return_type = Int; arguments = [Int]};
        };
        Pholyglot_ast.Function {
            name = "main";
            params = [];
            stmts = [
                Assignment (Int, Variable "b", Function_call (Function_type {return_type = Int; arguments = [Int]}, "foo", [Plus ((Num 10), (Num 20))]));
                Return (Plus (Variable "b", Num 30))
            ];
            function_type = Function_type {return_type = Int; arguments = []};
        }
    ] |> Pholyglot_ast.string_of_declares
    in
    [%test_eq: string] code {|#define function int
function foo(int $c)
#undef function
{
    return $c + 20;
}
#define function int
function main()
#undef function
{
    //?>
    int
    //<?php
    $b 
    = foo(10 + 20);
    return $b + 30;
}
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
        public float $y;
    }
    function main(): int {
        return 0;
    }
    |} in
    let linebuf = Lexing.from_string source in
    let ast = Parser.program Lexer.token linebuf in
    [%test_eq: Ast.program] ast (Declaration_list [
        Class {
            name= "Point";
            kind = Infer_kind;
            properties = [
                ("__object_property_x", Int);
                ("__object_property_y", Float);
            ];
            methods = [];
        };
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
        Class {
            name = "Point";
            kind = Infer_kind;
            properties = [
                ("__object_property_x", Int);
                ("__object_property_y", Int)
            ];
            methods = [];
        };
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
        Class {
            name = "Point";
            kind = Infer_kind;
            properties = [
                ("__object_property_x", Int);
                ("__object_property_y", Int)
            ];
            methods = [];
        };
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
        Class {
            name = "Point";
            kind = Infer_kind;
            properties = [
                ("__object_property_x", Int);
                ("__object_property_y", Int)
            ];
            methods = [];
        };
        Function {
            name = "main";
            docblock = [];
            params = [];
            stmts = [
                Assignment (Infer_me, Variable "p", (New (Class_type ("Point"), [])));
                Assignment (Infer_me, Object_access ("p", Property_access "__object_property_x"), (Num 1));
                Function_call (Infer_me, "printf", [String "\"%d\""; Object_access (Variable "p", Property_access "__object_property_x")]);
                Return (Num 0);
            ];
            function_type = Function_type {return_type = Int; arguments = []}
        }
    ])

let%test_unit "infer object access" =
    let ns = Namespace.create () in
    let ast : Ast.program = Ast.Declaration_list [
        Class {
            name = "Point";
            kind = Infer_kind;
            properties = [
                ("x", Int);
                ("y", Int);
            ];
            methods = [];
        };
        Function {
            name = "main";
            docblock = [];
            params = [];
            stmts = [
                Assignment (Infer_me, Variable "p", (New (Class_type ("Point"), [])));
                Assignment (Infer_me, Object_access ("p", Property_access "x"), (Num 1));
                Function_call (Infer_me, "printf", [String "\"%d\""; Object_access (Variable "p", Property_access "x")]);
                Return (Num 0);
            ];
            function_type = Function_type {return_type = Int; arguments = []}
        }
    ] |> Infer.run ns in
    [%test_eq: Ast.program] ast (Declaration_list [
        Class {
            name = "Point";
            kind = Val;
            properties = [
                ("x", Int);
                ("y", Int);
            ];
            methods = [];
        };
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
                    [Coerce (String_literal, String "\"%d\""); Object_access (Variable "p", Property_access "x")]
                );
                Return (Num 0)
            ];
            function_type = Function_type {return_type = Int; arguments = []}
        }
    ])

let%test_unit "output object access" =
    let code = [
        Ast.Class {
            name = "Point";
            kind = Val;
            properties = [
                ("__object_property_x", Int);
                ("__object_property_y", Int)
            ];
            methods = [];
        };
        Ast.Function {
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
                        Object_access (Variable "p", Property_access "__object_property_x")
                    ]
                );
                Return (Num 0)
            ];
            function_type = Function_type {return_type = Int; arguments = []}
        }
    ]
         |> Transpile.declarations_to_pholyglot
         |> Pholyglot_ast.string_of_declares
    in
    [%test_eq: string] code {|
//?>
typedef struct Point* Point;
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

    
// End of C struct def. Class methods are outside the struct.
//?>
};
//<?php

#if __PHP__
// End of PHP class def.
};
#endif
#if __PHP__
define("Point", "Point");
#endif
//?>
// Function pointer init
Point Point__constructor(Point $p)
{
    
    return $p;
}
//<?php
#define function int
function main()
#undef function
{
    //?>
    Point
    //<?php
    $p 
    = new(Point);
    $p->__object_property_x 
    = 1;
     printf("%d", $p->__object_property_x);
    return 0;
}
|}

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
        Class {
            name = "Thing";
            kind = Ref;
            properties = [
                ("__object_property_name", String);
            ];
            methods = [];
        };
        Function { 
            name = "foo";
            docblock = [];
            params = [Param ("t", Class_type "Thing")];
            stmts = [
                Function_call (
                    Function_type {return_type = Void; arguments = [String_literal; String_literal]},
                    "printf",
                    [Coerce (String_literal, String "\"%s\""); Coerce (String_literal, Object_access (Variable "t", Property_access "__object_property_name"))]
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

let%test_unit "docblock int" =
    let source = "<?php // @pholyglot
    /**
     * @param int $i
     */
    function foo(int $i): void {
    }
    " in

    let linebuf = Lexing.from_string source in
    let ast = Parser.program Lexer.token linebuf in
    [%test_eq: Ast.program] ast (Declaration_list [
        Function {
            name = "foo";
            docblock = [DocParam ("i", Int)];
            params = [Param ("i", Int)];
            stmts = [];
            function_type = Function_type {return_type = Void; arguments = [Int]}
        }
    ])

let%test_unit "docblock int array" =
    let source = "<?php // @pholyglot
    /**
     * @param array<int> $ints
     */
    function foo(array &$ints): void {
    }
    " in

    let linebuf = Lexing.from_string source in
    let ast = Parser.program Lexer.token linebuf in
    [%test_eq: Ast.program] ast (Declaration_list [
        Function {
            name = "foo";
            docblock = [DocParam ("ints", Dynamic_array (Int))];
            params = [RefParam ("ints", Fixed_array (Infer_me, None))];
            stmts = [];
            function_type = Function_type {return_type = Void; arguments = [Fixed_array (Infer_me, None)]}
        }
    ])

let%test_unit "infer docblock int array" =
    let source = "<?php // @pholyglot
    /**
     * @param array<int> $ints
     */
    function foo(array &$ints): void {
    }
    " in
    let ast =
        Lexing.from_string source |>
        Parser.program Lexer.token |>
        Infer.run (Namespace.create ())
    in
    [%test_eq: Ast.program] ast (Declaration_list [
        Function {
            name = "foo";
            docblock = [DocParam ("ints", Dynamic_array (Int))];
            params = [RefParam ("ints", Dynamic_array (Int))];
            stmts = [];
            function_type = Function_type {return_type = Void; arguments = [Dynamic_array (Int)]}
        }
    ])

let%test_unit "infer docblock string array" =
    let source = "<?php // @pholyglot
    /**
     * @param array<string> $strings
     */
    function foo(array &$strings): void {
    }
    " in
    let ast =
        Lexing.from_string source |>
        Parser.program Lexer.token |>
        Infer.run (Namespace.create ())
    in
    [%test_eq: Ast.program] ast (Declaration_list [
        Function {
            name = "foo";
            docblock = [DocParam ("strings", Dynamic_array (String))];
            params = [RefParam ("strings", Dynamic_array (String))];
            stmts = [];
            function_type = Function_type {return_type = Void; arguments = [Dynamic_array (String)]}
        }
    ])

let%test_unit "infer docblock object array" =
    let source = "<?php // @pholyglot
    class Body {}
    /**
     * @param array<Body> $bodies
     * @param float $dt
     */
    function foo(array &$bodies, float $dt): void {
    }
    " in
    let ast =
        Lexing.from_string source |>
        Parser.program Lexer.token |>
        Infer.run (Namespace.create ())
    in
    [%test_eq: Ast.program] ast (Declaration_list [
        Class {
            name = "Body";
            kind = Val;
            properties = [];
            methods = [];
        };
        Function {
            name = "foo";
            docblock = [
                DocParam ("bodies", Dynamic_array (Class_type "Body"));
                DocParam ("dt", Float);
            ];
            params = [
                RefParam ("bodies", Dynamic_array (Class_type "Body"));
                Param ("dt", Float);
            ];
            stmts = [];
            function_type = Function_type {return_type = Void; arguments = [Dynamic_array (Class_type "Body"); Float]}
        }
    ])

let%test_unit "transpile docblock object array" =
    let code =
        Ast.Function {
            name = "foo";
            docblock = [
                DocParam ("bodies", Dynamic_array (Class_type "Body"));
                DocParam ("dt", Float);
            ];
            params = [
                RefParam ("bodies", Dynamic_array (Class_type "Body"));
                Param ("dt", Float);
            ];
            stmts = [
                Assignment (Int, Variable "a", Num 123);
            ];
            function_type = Function_type {return_type = Void; arguments = [Dynamic_array (Class_type "Body"); Float]}
        }
         |> Transpile.declaration_to_pholyglot
         |> Pholyglot_ast.string_of_declare
    in
    [%test_eq: string] code {|//?>
void foo(array $bodies, float $dt)
//<?php
#if __PHP__
function foo(array &$bodies, float $dt): void
#endif
{
    //?>
    int
    //<?php
    $a 
    = 123;
    }
|}

let%test_unit "infer docblock int and string" =
    let source = "<?php // @pholyglot
    /**
     * @param string $string
     * @param int $int
     */
    function foo(string $string, int $int): void {
    }
    " in
    let ast =
        Lexing.from_string source |>
        Parser.program Lexer.token |>
        Infer.run (Namespace.create ())
    in
    [%test_eq: Ast.program] ast (Declaration_list [
        Function {
            name = "foo";
            docblock = [DocParam ("string", String); DocParam ("int", Int)];
            params = [Param ("string", String); Param ("int", Int)];
            stmts = [];
            function_type = Function_type {return_type = Void; arguments = [String; Int]}
        }
    ])

let%test_unit "method" =
    let source = "<?php // @pholyglot
class Point
{
    public int $x;
    public function getX(): int
    {
        return $this->x;
    }
}
    " in
    let ast =
        Lexing.from_string source |>
        Parser.program Lexer.token |>
        Infer.run (Namespace.create ())
    in
    [%test_eq: Ast.program] ast (Declaration_list [
        Class {
            name = "Point";
            kind = Val;
            properties = [("__object_property_x", Int)];
            methods = [
                {
                    name = "getX";
                    docblock = [];
                    params = [];
                    stmts = [
                        Return (Object_access (Variable "this", Property_access "__object_property_x"))
                    ];
                    function_type = Function_type {return_type = Int; arguments = []}
                }
            ]
        }
    ])

let%test_unit "simple getter" =
    let code = Ast.Class {
            name = "Point";
            kind = Val;
            properties = [("__object_property_x", Int)];
            methods = [
                {
                    name = "getX";
                    docblock = [];
                    params = [];
                    stmts = [
                        Return (Object_access (Variable "this", Property_access "__object_property_x"))
                    ];
                    function_type = Function_type {return_type = Int; arguments = []}
                }
            ]
        }
         |> Transpile.declaration_to_pholyglot
         |> Pholyglot_ast.string_of_declare
    in
    [%test_eq: string] code {|
//?>
typedef struct Point* Point;
//<?php
class Point {
    #define public int
#define __object_property_x $__object_property_x
    public $__object_property_x;
#undef public

    //?>
    int (*getX) (Point $self);
    //<?php
    
// End of C struct def. Class methods are outside the struct.
//?>
};
//<?php

//?>
int Point__getX (Point $self)
//<?php
#if __PHP__
public function getX(Point $self): int
#endif
{
    return $self->__object_property_x;

}

#if __PHP__
// End of PHP class def.
};
#endif
#if __PHP__
define("Point", "Point");
#endif
//?>
// Function pointer init
Point Point__constructor(Point $p)
{
    $p->getX = &Point__getX;

    return $p;
}
//<?php
|}

let%test_unit "infer method" =
    let source = {|<?php // @pholyglot
class Point
{
    public int $x;
    public function getX(): int
    {
        printf("Hello");
        return $this->x;
    }
}
    |} in
    let ast =
        Lexing.from_string source |>
        Parser.program Lexer.token |>
        Infer.run (Namespace.create ())
    in
    [%test_eq: Ast.program] ast (Declaration_list [
        Class {
            name = "Point";
            kind = Val;
            properties = [("__object_property_x", Int)];
            methods = [
                {
                    name = "getX";
                    docblock = [];
                    params = [];
                    stmts = [
                        Function_call (
                            Function_type {return_type = Void; arguments = [String_literal]},
                            "printf",
                            [Coerce (String_literal, String "\"Hello\"")]
                        );
                        Return (Object_access (Variable "this", Property_access "__object_property_x"))
                    ];
                    function_type = Function_type {return_type = Int; arguments = []}
                }
            ]
        }
    ])

let%test_unit "infer method call" =
    let ns = Namespace.create () in
    let c : Ast.declaration = Class {
        name = "Point";
        kind = Val;
        properties = [];
        methods = [
            {
                name          = "getX";
                docblock      = [];
                params        = [];
                stmts         = [];
                function_type = Function_type {return_type = Int; arguments = []}
            }
        ];
    } in
    Namespace.add_class_type ns c;
    Namespace.add_identifier ns "p" (Class_type "Point");
    let expr : Ast.expression = Object_access (Variable "p", Method_call {return_type = Infer_me; method_name = "getX"; args = []; left_hand = Variable "p"}) in
    let ast = Infer.infer_expression ns expr in
    [%test_eq: Ast.expression] ast (Object_access (Variable "p", Method_call {return_type = Int; method_name = "getX"; args = []; left_hand = Variable "p"}))

let%test_unit "infer method" =
    let source = {|<?php // @pholyglot
class Point
{
    public int $x;
    public function getX(): int
    {
        printf("Hello");
        return $this->x;
    }
}
function main(): int
{
    $p = new Point();
    printf("%d", $p->getX());
    return 0;
}
    |} in
    let ast =
        Lexing.from_string source |>
        Parser.program Lexer.token |>
        Infer.run (Namespace.create ())
    in
    [%test_eq: Ast.program] ast (Declaration_list [
        Class {
            name = "Point";
            kind = Val;
            properties = [("__object_property_x", Int)];
            methods = [
                {
                    name = "getX";
                    docblock = [];
                    params = [];
                    stmts = [
                        Function_call (
                            Function_type {return_type = Void; arguments = [String_literal]},
                            "printf",
                            [Coerce (String_literal, String "\"Hello\"")]
                        );
                        Return (Object_access (Variable "this", Property_access "__object_property_x"))
                    ];
                    function_type = Function_type {return_type = Int; arguments = []}
                }
            ]
        };
        Function {
            name = "main";
            docblock = [];
            params = [];
            stmts = [
                Assignment (Class_type "Point", Variable "p", (New (Class_type ("Point"), [])));
                Function_call (
                    Function_type {return_type = Void; arguments = [String_literal; Int]},
                    "printf",
                    [
                        Coerce (String_literal, String "\"%d\"");
                        Method_call {return_type = Int; method_name = "getX"; args = []; left_hand = Variable "p"};
                    ]
                );
                Return (Num 0)
            ];
            function_type = Function_type {return_type = Int; arguments = []}
        }
    ])

let%test_unit "transpile method" =
    let ast : Ast.expression =
        Function_call (
            Function_type {return_type = Void; arguments = [String_literal; Int]},
            "printf",
            [
                Coerce (String_literal, String "\"%d\"");
                Method_call {
                    return_type = Int;
                    method_name = "getX";
                    args = [
                        Variable "var1";
                        Function_call (
                            Function_type {return_type = Void; arguments = []},
                            "moo",
                            []
                        );
                        Array_access ("arr", Num 0);
                    ];
                    left_hand = Variable "p";
                }
            ]
        )
    in
    let phast = Transpile.expression_to_pholyglot ast in
    let code = Pholyglot_ast.string_of_expression phast in
    [%test_eq: string] code {|pprintf("%d", $p->getX($p, $var1, moo(), $arr[0]))|}

let%test_unit "float test" =
    let source = "<?php // @pholyglot
    function main(): int {
        $a = 1.0 + 2.0 - 3.25;
        return 0;
    }
    " in
    let ast =
        Lexing.from_string source |>
        Parser.program Lexer.token |>
        Infer.run (Namespace.create ())
    in
    [%test_eq: Ast.program] ast (Ast.Declaration_list [
        Function {
            name= "main";
            docblock = [];
            params = [];
            stmts = [
                Assignment (Float, Variable "a", (Minus (Plus ((Num_float 1.0), (Num_float 2.0)), (Num_float 3.25))));
                Return (Num 0);
            ];
            function_type = Function_type {return_type = Int; arguments = []}
        }
    ])

let%test "float int conflict test" =
    let source = "<?php // @pholyglot
    function main(): int {
        $a = 1.0 + 2.0 - 3;
        return 0;
    }
    " in
    match
        Lexing.from_string source |>
        Parser.program Lexer.token |>
        Infer.run (Namespace.create ())
    with
    | exception Infer.Type_error _ -> true
    | _ -> false

let%test_unit "float to code test" =
    let fn : Ast.declaration = 
        Function {
            name= "main";
            docblock = [];
            params = [];
            stmts = [
                Assignment (Float, Variable "a", (Minus (Plus ((Num_float 1.0), (Num_float 2.0)), (Num_float 3.25))));
                Return (Num 0);
            ];
            function_type = Function_type {return_type = Int; arguments = []}
        }
    in
    let phast = Transpile.declaration_to_pholyglot fn in
    let code = Pholyglot_ast.string_of_declare phast in
    [%test_eq: string] code {|#define function int
function main()
#undef function
{
    //?>
    float
    //<?php
    $a 
    = 1. + 2. - 3.25;
    return 0;
}
|}

let%test_unit "printf float" =
    let source = {|<?php // @pholyglot
    function foo(): void {
        $a = 1.0 + 2.5;
        printf("%f", $a);
    }
    |} in
    let ast =
        Lexing.from_string source |>
        Parser.program Lexer.token |>
        Infer.run (Namespace.create ())
    in
    [%test_eq: Ast.program] ast (Declaration_list [
        Function {
            name = "foo";
            docblock = [];
            params = [];
            stmts = [
                Assignment (Float, Variable "a", (Plus ((Num_float 1.0), (Num_float 2.5))));
                Function_call (
                    Function_type {return_type = Void; arguments = [String_literal; Float]},
                    "printf",
                    [Coerce (String_literal, String "\"%f\""); Variable ("a")];
                );
            ];
            function_type = Function_type {return_type = Void; arguments = []}
        }
    ])

let%test_unit "object access inside array access" =
    let source = {|<?php // @pholyglot
    class Body {public int $x;}
    function foo(): void {
        $b = new Body();
        $b->x = 10;
        $arr = [$b];
        $x = $arr[0]->x;
    }
    |} in
    let ast =
        Lexing.from_string source |>
        Parser.program Lexer.token |>
        Infer.run (Namespace.create ())
    in
    [%test_eq: Ast.program] ast (Declaration_list [
        Class {
            name = "Body";
            kind = Val;
            properties = [("__object_property_x", Int)];
            methods = []
        };
        Function {
            name = "foo";
            docblock = [];
            params = [];
            stmts = [
                Assignment (Class_type "Body", Variable "b", New (Class_type "Body", []));
                Assignment (Int, Object_access ("b", (Property_access "__object_property_x")), (Num 10));
                Assignment (Fixed_array (Class_type "Body", Some 1), (Variable "arr"),
                    Function_call (
                        Function_type {return_type = Fixed_array (Class_type "Body", Some 1); arguments = [Constant; Int; Class_type "Body"]},
                        "array_make",
                        [Constant "Body"; Num 1; Variable "b"];
                    )
                );
                Assignment (Int, Variable "x", Object_access (Array_access ("arr", Num 0), Property_access "__object_property_x"));
            ];
            function_type = Function_type {return_type = Void; arguments = []}
        }
    ])

let%test_unit "object access inside array access transpile" =
    let fn : Ast.declaration = Function {
            name = "foo";
            docblock = [];
            params = [];
            stmts = [
                Assignment (Class_type "Body", Variable "b", New (Class_type "Body", []));
                Assignment (Int, Object_access ("b", (Property_access "__object_property_x")), (Num 10));
                Assignment (Fixed_array (Class_type "Body", Some 1), (Variable "arr"),
                    Function_call (
                        Function_type {return_type = Fixed_array (Class_type "Body", Some 1); arguments = [Constant; Int; Class_type "Body"]},
                        "array_make",
                        [Constant "Body"; Num 1; Variable "b"];
                    )
                );
                Assignment (Int, Variable "x", Object_access (Array_access ("arr", Num 0), Property_access "__object_property_x"));
            ];
            function_type = Function_type {return_type = Void; arguments = []}
        }
    in
    let phast = Transpile.declaration_to_pholyglot fn in
    let code = Pholyglot_ast.string_of_declare phast in
	(* TODO: Check this code *)
    [%test_eq: string] code {|#define function void
function foo()
#undef function
{
    //?>
    Body
    //<?php
    $b 
    = new(Body);
    $b->__object_property_x 
    = 10;
    //?>
    array
    //<?php
    $arr 
    = array_make(Body, 1, $b);
    //?>
    int
    //<?php
    $x 
    = $arr[0]->__object_property_x;
    }
|}

let%test_unit "negative int" =
    let source = {|<?php // @pholyglot
    function foo(): void {
        $x = -10 - -5;
        $y = 10 * -10;
        $f = -1.15 / -2.;
    }
    |} in
    let ast =
        Lexing.from_string source |>
        Parser.program Lexer.token |>
        Infer.run (Namespace.create ())
    in
    [%test_eq: Ast.program] ast (Declaration_list [
        Function {
            name = "foo";
            docblock = [];
            params = [];
            stmts = [
                Assignment (Int, Variable "x", Minus (Num (-10), Num (-5)));
                Assignment (Int, Variable "y", Times (Num 10, Num (-10)));
                Assignment (Float, Variable "f", Div (Num_float (-1.15), Num_float (-2.)));
            ];
            function_type = Function_type {return_type = Void; arguments = []}
        }
    ])


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
(* array_get - or extra variable? *)
(* type-cast of int and float *)
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
(* Nullable types and refinement *)
(* All props must have default values. That's not null? *)
(* Parse class property docblock *)
(* Ban $self variable name, since it's used internally instead of $this *)
(* inline "new" but must init function pointers? *)
(* typedef to get rid of struct Point* $self C-only code *)
(* Infer return type for methods *)
(* Object access method call *)
(* Method pointer init *)
(* Property defaults *)
