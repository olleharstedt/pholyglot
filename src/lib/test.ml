open Printf
module Log = Dolog.Log

(*
let levenshtein s t =
   let rec dist i j = match (i,j) with
      | (i,0) -> i
      | (0,j) -> j
      | (i,j) ->
         if s.[i-1] = t.[j-1] then dist (i-1) (j-1)
         else let d1, d2, d3 = dist (i-1) j, dist i (j-1), dist (i-1) (j-1) in
         1 + min d1 (min d2 d3)
   in
   dist (String.length s) (String.length t)
*)

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
    [%test_eq: Base.string] pholyglot_code {|#define function int
function main()
#undef function
{
    #__C__ GC_INIT();
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
    [%test_eq: Base.string] pholyglot_code {|#define function int
function main()
#undef function
{
    #__C__ GC_INIT();
#__C__ int
    $a = 0;
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
    [%test_eq: Base.string] pholyglot_code {|//<?php echo "\x08\x08"; ob_start(); ?>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <glib.h>
#include <math.h>
#include <phollylib.c>
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
    #__C__ GC_INIT();
#__C__ GString*
    $str = g_string_append(g_string_append(g_string_new("Hello"), g_string_new(" world")->str), g_string_new("!")->str);
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
    [%test_eq: Base.string] code {|#define function int
function foo(int $c)
#undef function
{
    return $c + 20;
}
#define function int
function main()
#undef function
{
    #__C__ GC_INIT();
#__C__ int
    $b = foo(10 + 20);
    return $b + 30;
}
|}

let%test_unit "infer_printf 1" =
    let t = Infer.infer_printf "%d" in
    [%test_eq: Ast.typ Base.list] t [Ast.Int]

let%test_unit "infer_printf 2" =
    let t = Infer.infer_printf "%s" in
    [%test_eq: Ast.typ Base.list] t [Ast.String_literal]

let%test_unit "infer_printf 3" =
    let t = Infer.infer_printf "%s %d" in
    [%test_eq: Ast.typ Base.list] t [Ast.String_literal; Ast.Int]

let%test_unit "infer_printf 4" =
    let t = Infer.infer_printf "Bla bla $something !!! %s moo foo %deee" in
    [%test_eq: Ast.typ Base.list] t [Ast.String_literal; Ast.Int]

let%test_unit "float printf" =
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
                        Coerce (String_literal, String "\"%s %ld\"");
                        Coerce (String_literal, String "\"Hello\"");
                        Num 1
                    ]
                );
                Return (Num 0);
            ];
            function_type = Function_type {return_type = Int; arguments = []}
        }
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
    [%test_eq: Base.string] code {|#define function int
function main()
#undef function
{
    #__C__ GC_INIT();
#__C__ float
    $a = 1. + 2. - 3.25;
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

let%test_unit "infer foreach" =
    let source = {|<?php // @pholyglot
    function foo(): void {
        $arr = [1, 2, 3];
        foreach ($arr as $val) {
            printf("%d", $val);
        }
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
                Assignment (Fixed_array (Int, Some 3), Variable "arr", 
                    Function_call (
                        Function_type {return_type = Fixed_array (Int, Some 3); arguments = [Constant; Int; Int; Int; Int]},
                        "array_make",
                        [Constant "int"; Num 3; Num 1; Num 2; Num 3]
                    )
                );
                Foreach {
                    arr = Variable "arr";
                    key = None; 
                    value = Variable "val";
                    value_typ = Int;
                    value_typ_constant = Constant "int";
                    body = [
                        Function_call (
                            Function_type {return_type = Void; arguments = [String_literal; Int]},
                            "printf",
                            [
                                Coerce (String_literal, String "\"%ld\"");
                                Variable "val";
                            ]
                        );
                    ];
                };
            ];
            function_type = Function_type {return_type = Void; arguments = []}
        }
    ])

(* TODO: Also do for $key => $val *)
let%test_unit "foreach to pholyglot" =
    let ast = Ast.Foreach {
        arr = Variable "arr";
        key = None; 
        value = Variable "val";
        value_typ = Int;
        value_typ_constant = Constant "int";
        body = [
            Function_call (
                Function_type {return_type = Void; arguments = [String_literal; Int]},
                "printf",
                [
                    Coerce (String_literal, String "\"%ld\"");
                    Variable "val";
                ]
            );
        ];
    } in
    let phast = Transpile.statement_to_pholyglot ast in
    let code = Pholyglot_ast.string_of_statement phast in
    [%test_eq: Base.string] code {|#__C__ int
    $__i = 0;
    
        for (; $__i < count($arr); $__i = $__i + 1) {
            #__C__ int
    $val = array_get(int, $arr, $__i);
     printf("%ld", $val);
    
        }
    |}

let%test_unit "infer foreach with key" =
    let source = {|<?php // @pholyglot
    function foo(): void {
        $arr = [2, 3, 4];
        foreach ($arr as $i => $val) {
            printf("%d", $i);
        }
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
                Assignment (Fixed_array (Int, Some 3), Variable "arr", 
                    Function_call (
                        Function_type {return_type = Fixed_array (Int, Some 3); arguments = [Constant; Int; Int; Int; Int]},
                        "array_make",
                        [Constant "int"; Num 3; Num 2; Num 3; Num 4]
                    )
                );
                Foreach {
                    arr = Variable "arr";
                    key = Some (Variable "i");
                    value = Variable "val";
                    value_typ = Int;
                    value_typ_constant = Constant "int";
                    body = [
                        Function_call (
                            Function_type {return_type = Void; arguments = [String_literal; Int]},
                            "printf",
                            [
                                Coerce (String_literal, String "\"%ld\"");
                                Variable "i";
                            ]
                        );
                    ];
                };
            ];
            function_type = Function_type {return_type = Void; arguments = []}
        }
    ])

let%test_unit "plusplus and minusminus" =
    let source = {|<?php // @pholyglot
    function foo(): void {
        $a = 10;
        $a++;
        $a--;
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
            function_type = Function_type {return_type = Void; arguments = []};
            stmts = [
                Assignment (Int, Variable "a", Num 10);
                Plusplus (Variable "a");
                Minusminus (Variable "a");
            ]
        }
    ])

let%test_unit "plusplus and minusminus pholyglot" =
    let ast : Ast.declaration = Function {
            name = "foo";
            docblock = [];
            params = [];
            function_type = Function_type {return_type = Void; arguments = []};
            stmts = [
                Assignment (Int, Variable "a", Num 10);
                Plusplus (Variable "a");
                Minusminus (Variable "a");
            ]
        }
    in
    let phast = Transpile.declaration_to_pholyglot ast in
    let code = Pholyglot_ast.string_of_declare phast in
    [%test_eq: Base.string] code {|#define function void
function foo()
#undef function
{
    #__C__ int
    $a = 10;
    $a++;
$a--;
}
|}

let%test_unit "pluseq and minuseq" =
    let source = {|<?php // @pholyglot
    function foo(): void {
        $a = 10;
        $a += 10;
        $a -= (10 * 2);
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
            function_type = Function_type {return_type = Void; arguments = []};
            stmts = [
                Assignment (Int, Variable "a", Num 10);
                Pluseq (Variable "a", Num 10);
                Minuseq (Variable "a", Parenth (Times (Num 10, Num 2)));
            ]
        }
    ])

let%test_unit "pluseq and minuseq pholyglot" =
    let fn = Ast.Function {
        name = "foo";
        docblock = [];
        params = [];
        function_type = Function_type {return_type = Void; arguments = []};
        stmts = [
            Assignment (Int, Variable "a", Num 10);
            Pluseq (Variable "a", Num 10);
            Minuseq (Variable "a", Parenth (Times (Num 10, Num 2)));
        ]
    }
    in
    let phast = Transpile.declaration_to_pholyglot fn in
    let code = Pholyglot_ast.string_of_declare phast in
    [%test_eq: Base.string] code {|#define function void
function foo()
#undef function
{
    #__C__ int
    $a = 10;
    $a += 10;
$a -= (10 * 2);
}
|}

let%test_unit "do while" =
    let source = {|<?php // @pholyglot
    function foo(): void {
        $a = 10;
        do {
            $a--;
        } while($a > 0);
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
            function_type = Function_type {return_type = Void; arguments = []};
            stmts = [
                Assignment (Int, Variable "a", Num 10);
                Dowhile {
                    body = [
                        Minusminus (Variable "a");
                    ];
                    condition = Greaterthan (Variable "a", Num 0);
                };
            ]
        }
    ])

let%test_unit "do while pholyglot" =
    let fn = Ast.Function {
        name = "foo";
        docblock = [];
        params = [];
        function_type = Function_type {return_type = Void; arguments = []};
        stmts = [
            Assignment (Int, Variable "a", Num 10);
            Dowhile {
                body = [
                    Minusminus (Variable "a");
                ];
                condition = Greaterthan (Variable "a", Num 0);
            };
        ]
    }
    in
    let phast = Transpile.declaration_to_pholyglot fn in
    let code = Pholyglot_ast.string_of_declare phast in
    [%test_eq: Base.string] code {|#define function void
function foo()
#undef function
{
    #__C__ int
    $a = 10;
    
do {
$a--;
} while ($a > 0);
}
|}

let%test_unit "void return type" =
    let source = {|<?php // @pholyglot
    function foo(): void {
    }
    function main(): int {
        foo();
        return 0;
    }
    |} in
    let ns = Namespace.create () in
    let ast =
        Lexing.from_string source |>
        Parser.program Lexer.token |>
        Infer.run (Namespace.create ()) |>
        Transpile.run
    in
    ()

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
(* Write shorter test for assignment + new + boehm in new test file; don't use inline tests? *)
(* SplDoublyLinkedList as dlist, $list = new SplDoublyLinkedList(); $list[] = $item; *)
(*   foreach works with SplDoublyLinkedList --> transpile to do-while $list->valid()
 *   $list->valid() - must pass self in C but not PHP - special typ of method? "Implied self" *)
(* @memory-context $argument1 for function docblock *)
(* Are there other ways than "new" to alloc mem? *)
(* Memory_context for variable or typ *)
(* Do types need Memory_context? How to know what to return from a function? Memory-polymorph? *)
(* fgets for input *)
(* foreach ([1, 2, 3] as $i) *)
(* array_slice is polymorph on both memory and input type *)
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
(* Only one return statement per function allowed, must be last statement. Unless void type? *)
(* Dynamic string on heap vs fixed string on stack (only works for string literal) *)
(* //21:42 < fizzie> struct Point *p = (struct Point[]){foo()};  // just to be silly *)
(* Define memory model by interface? Pholly\Memory\RefCount. Or better to inject in constructor, to not lock it in? But then runtime? Or must be possible to infer at compile time? *)
(* Cannot only use void-function as statements (no ignore()?) *)
(* Interaction between val types and ref types? Most coerce val types to ref when added? That is, copy mem to correct box. *)
(* Chaining property access, like $rectangle->point->x *)
(* Nullable types and refinement *)
(* All props must have default values. That's not null? *)
(* Parse class property docblock *)
(* Infer return type for methods *)
(* Object access method call *)
(* Method pointer init *)
(* Infer ++ and -- only on int and float *)
(* Forbid aliasing for malloc/free from external libs (mysqli etc); some type of uniqueness *)
(* Create composer package that install binary on Linux. https://github.com/bamarni/composer-bin-plugin https://github.com/ENM1989/chromedriver *)
(* Use dune watch file to auto run tests *)
(* Configure dune to run both inline test and ounit *)
(* A-star/A* algorithm with arena allocator? *)
(* Rename mem to __mem for internal class variable *)
