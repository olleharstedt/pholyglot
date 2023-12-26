open Printf
module Log = Dolog.Log

(**
 * Malloc as an alloc type is needed to deal with third-party library functions like file_get_contents.
 *)

let%test_unit "basic clone" =
    let source = {|<?php // @pholyglot
function foo(): void
{
    $p1 = new Point();
    $p2 = clone $p1;
}
|}
    in
    let linebuf = Lexing.from_string source in
    let ns = Namespace.create () in
    let ast = Parser.program Lexer.token linebuf |> Infer.run ns in
    [%test_eq: Ast.program] ast (Declaration_list [
        Function {
            name = "foo";
            docblock = [];
            params = [];
            stmts = [
                Assignment (
                    Class_type ("Point", Boehm),
                    Variable "p1",
                    New (
                        None,
                        Class_type ("Point", Boehm),
                        []
                    );
                );
                Assignment (
                    Class_type ("Point", Boehm),
                    Variable "p2",
                    Clone {
                        variable_name = "p1";
                        t             = Class_type ("Point", Boehm);
                        alloc_strat   = Some Boehm
                    }
                );
            ];
            function_type = Function_type {return_type = Void; arguments = []; uses_arena = false}
        }
    ])

let%test_unit "basic clone src" =
    let code = [
        Ast.Function {
            name = "foo";
            docblock = [];
            params = [];
            stmts = [
                Assignment (
                    Class_type ("Point", Boehm),
                    Variable "p1",
                    New (
                        None,
                        Class_type ("Point", Boehm),
                        []
                    );
                );
                Assignment (
                    Class_type ("Point", Boehm),
                    Variable "p2",
                    Clone {
                        variable_name = "p1";
                        t             = Class_type ("Point", Boehm);
                        alloc_strat   = Some Boehm;
                    }
                );
            ];
            function_type = Function_type {return_type = Void; arguments = []; uses_arena = false}
        }
    ]
         |> Transpile.declarations_to_pholyglot
         |> Pholyglot_ast.string_of_declares
    in
    [%test_eq: Base.string] code {|#define function void
function foo()
#undef function
{
    #__C__ Point
    $p1 = new(Point
#__C__, gc_mem
);
    #__C__ Point
    $p2 = clone($p1
#__C__, Point, gc_mem
);
    }
|}


(* TODO
 * Support clone with @alloc annotation
 *)
