module Log = Dolog.Log

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
                        Coerce (String_literal, String "\"%ld\"");
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
    let fn = Infer.infer_declaration fn (Namespace.create ()) in
    let phast = Transpile.declaration_to_pholyglot fn in
    let pholyglot_code = Pholyglot_ast.string_of_declare phast in
    [%test_eq: Base.string] pholyglot_code {|#define function int
function main()
#undef function
{
    #__C__ GC_INIT();
#__C__ array
    $arr = array_make(int, 3, 1, 2, 3);
     printf("%ld", array_get(int, $arr, 0));
    return 0;
}
|}

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
            builtin_class = false;
        };
        Function {
            name = "foo";
            docblock = [
                DocParam ("bodies", Dynamic_array (Class_type ("Body", Memory_polymorph)));
                DocParam ("dt", Float);
            ];
            params = [
                RefParam ("bodies", Dynamic_array (Class_type ("Body", Memory_polymorph)));
                Param ("dt", Float);
            ];
            stmts = [];
            function_type = Function_type {return_type = Void; arguments = [Dynamic_array (Class_type ("Body", Memory_polymorph)); Float]}
        }
    ])

let%test_unit "transpile docblock object array" =
    let code =
        Ast.Function {
            name = "foo";
            docblock = [
                DocParam ("bodies", Dynamic_array (Class_type ("Body", Boehm)));
                DocParam ("dt", Float);
            ];
            params = [
                RefParam ("bodies", Dynamic_array (Class_type ("Body", Boehm)));
                Param ("dt", Float);
            ];
            stmts = [
                Assignment (Int, Variable "a", Num 123);
            ];
            function_type = Function_type {return_type = Void; arguments = [Dynamic_array (Class_type ("Body", Boehm)); Float]}
        }
         |> Transpile.declaration_to_pholyglot
         |> Pholyglot_ast.string_of_declare
    in
    [%test_eq: Base.string] code {|#__C__ void foo(array $bodies, float $dt)
#if __PHP__
function foo(array &$bodies, float $dt): void
#endif
{
    #__C__ int
    $a = 123;
    }
|}

let%test_unit "array slice test" =
    Log.set_log_level Log.DEBUG;
    (* Location becomes ./_build/default/lib/debug.txt *)
    Log.set_output (open_out "debug.txt");
    Log.debug "array slice test";
    let source = {|<?php // @pholyglot
    function foo(): void {
        $arr = [1, 2, 3];
        $arr2 = array_slice($arr, 1);
        $i = $arr2[0];
        printf("%d", $i);
    }
    |} in
    let ast =
        Lexing.from_string source |>
        Parser.program Lexer.token |>
        Infer.run (Namespace.create ())
    in
    Log.set_log_level Log.FATAL;
    Log.clear_prefix ();
    Log.debug "should not be visible";
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
                Assignment (Fixed_array (Int, Some 3), Variable "arr2", 
                    Function_call (
                        Function_type {return_type = Fixed_array (Int, Some 3); arguments = [Fixed_array (Int, Some 3); Int]},
                        "array_slice",
                        [Variable "arr"; Num 1]
                    )
                );
                Assignment (Int, Variable "i",
                    Function_call (
                        Function_type {return_type = Int; arguments = [Constant; Dynamic_array Int; Int]},
                        "array_get",
                        [Constant "int"; Variable "arr2"; Num 0]
                    )
                );
                Function_call (
                    Function_type {return_type = Void; arguments = [String_literal; Int]},
                    "printf",
                    [
                        Coerce (String_literal, String "\"%ld\"");
                        Variable "i";
                    ]
                );
            ];
            function_type = Function_type {return_type = Void; arguments = []}
        }
    ])

let%test_unit "array slice pholyglot code" =
    let ast : Ast.declaration = Function {
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
                Assignment (Fixed_array (Int, Some 3), Variable "arr2", 
                    Function_call (
                        Function_type {return_type = Fixed_array (Int, Some 3); arguments = [Fixed_array (Int, Some 3); Int]},
                        "array_slice",
                        [Variable "arr"; Num 1]
                    )
                );
                Assignment (Int, Variable "i",
                    Function_call (
                        Function_type {return_type = Int; arguments = [Constant; Dynamic_array Int; Int]},
                        "array_get",
                        [Constant "int"; Variable "arr2"; Num 0]
                    )
                );
                Function_call (
                    Function_type {return_type = Void; arguments = [String_literal; Int]},
                    "printf",
                    [
                        Coerce (String_literal, String "\"%ld\"");
                        Variable "i";
                    ]
                );
            ];
            function_type = Function_type {return_type = Void; arguments = []}
        }
    in
    let phast = Transpile.declaration_to_pholyglot ast in
    let code = Pholyglot_ast.string_of_declare phast in
    [%test_eq: Base.string] code {|#define function void
function foo()
#undef function
{
    #__C__ array
    $arr = array_make(int, 3, 1, 2, 3);
    #__C__ array
    $arr2 = array_slice($arr, 1);
    #__C__ int
    $i = array_get(int, $arr2, 0);
     printf("%ld", $i);
    }
|}

