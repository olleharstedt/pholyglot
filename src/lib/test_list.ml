
let%test_unit "trivial list" =
    let source = {|<?php // @pholyglot
    function foo(): void {
        $list = new SplDoublyLinkedList();
    }
    |} in
    let linebuf = Lexing.from_string source in
    let ast = Parser.program Lexer.token linebuf in
    [%test_eq: Ast.program] ast (Declaration_list [
        Function {
            name = "foo";
            docblock = [];
            params = [];
            stmts = [
                Assignment (Infer_me, Variable "list", New (None, Infer_me, [List_init (Infer_me)]));
            ];
            function_type = Function_type {return_type = Void; arguments = []}
        }
    ])

let%test_unit "trivial list infer" =
    let source = {|<?php // @pholyglot
    function foo(): void {
        /** @var SplDoublyLinkedList<int> */
        $list = new SplDoublyLinkedList();
    }
    |} in
    let linebuf = Lexing.from_string source in
    let ns = Namespace.create () in
    let ast = Parser.program Lexer.token linebuf |> Infer.run ns in
    [%test_eq: Ast.program] ast (Declaration_list [
        Function {
            name = "foo";
            docblock = [];
            params = [];
            stmts = [
                Assignment (List Int, Variable "list", New (None, List Int, [List_init (List Int)]));
            ];
            function_type = Function_type {return_type = Void; arguments = []}
        }
    ])

let%test_unit "trivial list infer 2" =
    let source = {|<?php // @pholyglot
    function foo(): void {
        /** @var SplDoublyLinkedList<Point> */
        $list = new SplDoublyLinkedList();
    }
    |} in
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
                    List (Class_type ("Point", Boehm)),
                    Variable "list",
                    New (None, Class_type ("Point", Boehm), [List_init (List (Class_type ("Point", Boehm)))])
                    (*Assignment (List Int, Variable "list", New (None, List Int, [List_init (List Int)]));*)
                );
            ];
            function_type = Function_type {return_type = Void; arguments = []}
        }
    ])

(*
let%test_unit "trivial list infer class and allocation strat" =
    let source = {|<?php // @pholyglot
    function foo(): void {
        /** @var SplDoublyLinkedList<Point> */
        $list = /** @alloc arena */ new SplDoublyLinkedList();
    }
    |} in
    let linebuf = Lexing.from_string source in
    let ns = Namespace.create () in
    let ast = Parser.program Lexer.token linebuf |> Infer.run ns in
    [%test_eq: Ast.program] ast (Declaration_list [
        Function {
            name = "foo";
            docblock = [];
            params = [];
            stmts = [
                Assignment (List (Class_type ("Point", Infer_allocation_strategy)), Variable "list", List_init (List (Class_type ("Point", Infer_allocation_strategy))));
            ];
            function_type = Function_type {return_type = Void; arguments = []}
        }
    ])
*)

(**
TODO:
    Conflicting @var
 *)
