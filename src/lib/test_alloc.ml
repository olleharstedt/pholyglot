open Printf
module Log = Dolog.Log

let%test_unit "alloc boehm" =
    let source = {|<?php // @pholyglot
    class Point {}
    function foo(): void {
        $p = /** @alloc boehm */ new Point();
    }
    |} in
    let linebuf = Lexing.from_string source in
    let ast = Parser.program Lexer.token linebuf in
    [%test_eq: Ast.program] ast (Declaration_list [
        Class {
            name = "Point";
            kind = Infer_kind;
            properties = [];
            methods = [];
        };
        Function {
            name = "foo";
            docblock = [];
            params = [];
            stmts = [
                Assignment (Infer_me, Variable "p", (New (Some Boehm, Class_type ("Point", Boehm), [])));
            ];
            function_type = Function_type {return_type = Void; arguments = []}
        }
    ])

let%test_unit "alloc arena" =
    let source = {|<?php // @pholyglot
    class Point {}
    function foo(): void {
        $p = /** @alloc arena */ new Point();
    }
    |} in
    let linebuf = Lexing.from_string source in
    let ast = Parser.program Lexer.token linebuf in
    [%test_eq: Ast.program] ast (Declaration_list [
        Class {
            name = "Point";
            kind = Infer_kind;
            properties = [];
            methods = [];
        };
        Function {
            name = "foo";
            docblock = [];
            params = [];
            stmts = [
                Assignment (Infer_me, Variable "p", (New (Some Arena, Class_type ("Point", Arena), [])));
            ];
            function_type = Function_type {return_type = Void; arguments = []}
        }
    ])

let%test_unit "alloc stack" =
    let source = {|<?php // @pholyglot
    class Point {}
    function foo(): void {
        $p = /** @alloc stack */ new Point();
    }
    |} in
    let linebuf = Lexing.from_string source in
    let ast = Parser.program Lexer.token linebuf in
    [%test_eq: Ast.program] ast (Declaration_list [
        Class {
            name = "Point";
            kind = Infer_kind;
            properties = [];
            methods = [];
        };
        Function {
            name = "foo";
            docblock = [];
            params = [];
            stmts = [
                Assignment (Infer_me, Variable "p", (New (Some Stack, Class_type ("Point", Stack), [])));
            ];
            function_type = Function_type {return_type = Void; arguments = []}
        }
    ])

let%test "faulty alloc type" =
    let source = {|<?php // @pholyglot
    class Point {}
    function foo(): void {
        $p = /** @alloc moo */ new Point();
    }
    |} in
    let linebuf = Lexing.from_string source in
    try 
        ignore(Parser.program Lexer.token linebuf);
        false
    with
         | Ast.DocblockParseError _ -> true
         | _ -> false

(* TODO
$p = /** @alloc $list */ new Point(); // To use an existing context
$p = /** @alloc stack */ new Point() + 1;
$p = /** @alloc stack */ [new Point()]; // Not valid?
return /** @alloc arena */ new Point();
default to Boehm

*)
