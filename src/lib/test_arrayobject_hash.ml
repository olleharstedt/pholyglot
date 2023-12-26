open Printf
module Log = Dolog.Log

let%test_unit "trivial arrayobject" =
    let source = {|<?php // @pholyglot
    function main(): int {
        /** @var array<string, int> */
        $hash = new ArrayObject();
        return 0;
    }
    |}
    in
    let linebuf = Lexing.from_string source in
    let ast = Parser.program Lexer.token linebuf in
    [%test_eq: Ast.program] ast (Declaration_list [
        Function {
            name = "main";
            docblock = [];
            params = [];
            stmts = [
                Assignment (
                    Hash_table (String, Int),
                    Variable "hash",
                    New (None, Infer_me, [Hash_init (Infer_me)])
                );
                Return (Num 0);
            ];
            function_type = Function_type {return_type = Int; arguments = []; uses_arena = false}
        }
    ])

let%test_unit "trivial arrayobject infer" =
    let fn =
        Ast.Function {
            name = "main";
            docblock = [];
            params = [];
            stmts = [
                Assignment (
                    Hash_table (String, Int),
                    Variable "hash",
                    New (None, Infer_me, [Hash_init (Infer_me)])
                );
                Return (Num 0);
            ];
            function_type = Function_type {return_type = Int; arguments = []; uses_arena = false}
        }
    in
    let ns = Namespace.create () in
    let ast = Infer.infer_declaration fn ns in
    [%test_eq: Ast.declaration] ast
        (Ast.Function {
            name = "main";
            docblock = [];
            params = [];
            stmts = [
                Assignment (
                    Hash_table (String, Int),
                    Variable "hash",
                    New (None, Hash_table (String, Int), [Hash_init (Hash_table (String, Int))])
                );
                Return (Num 0);
            ];
            function_type = Function_type {return_type = Int; arguments = []; uses_arena = false}
        })

(* TODO
 * alloc type
 * element alloc type?
 *)
