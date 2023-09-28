open Printf
module Log = Dolog.Log

(**
 * Malloc as an alloc type is needed to deal with third-party library functions like file_get_contents.
 *)

let%test_unit "basic alloc heap" =
    let source = {|<?php // @pholyglot
function foo(): void
{
    $p = /** @alloc heap */ new Point();
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
                    Variable "p",
                    New (
                        Some Heap,
                        Class_type ("Point", Heap),
                        []
                    );
                );
            ];
            function_type = Function_type {return_type = Void; arguments = []; uses_arena = false}
        }
    ])

let%test_unit "basic alloc heap src" =
    let code = [
        Ast.Function {
            name = "foo";
            docblock = [];
            params = [];
            stmts = [
                Assignment (
                    Class_type ("Point", Boehm),
                    Variable "p",
                    New (
                        Some Heap,
                        Class_type ("Point", Heap),
                        []
                    );
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
    $p = new(Point
#__C__, heap_mem
);
    }
|}

(* TODO
 * malloc is not allowed to escape
 *   shortcut without escape analysis? Not allowed on right-hand side without a mem clone/copy
 *)
