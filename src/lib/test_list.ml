
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
                    (* TODO: Should None be Some Boehm? Needed? Duplication? *)
                    New (
                        None,
                        List (Class_type ("Point", Boehm)),
                        [List_init (List (Class_type ("Point", Boehm)))]
                    )
                );
            ];
            function_type = Function_type {return_type = Void; arguments = []}
        }
    ])

let%test_unit "trivial list infer class and allocation strat" =
    let source = {|<?php // @pholyglot
    function foo(): void {
        /** @var SplDoublyLinkedList<Point> */
        $list = /** @alloc arena */ new SplDoublyLinkedList();
    }
    |} in
    (*
#define function void
function foo()
#undef function
{
    #__C__ Arena __a = malloc(sizeof(struct Arena));
    #__C__ arena_init(__a, malloc(256), 256);
    #__C__ arena_mem.alloc = &arena_alloc;
    #__C__ arena_mem.arena = __a;
    #__C__ gc_mem.alloc = &gc_malloc;
    #__C__ gc_mem.arena = NULL;

    #__C__ SplDoublyLinkedList
    $list = new(SplDoublyLinkedList
        #__C__, arena_mem
    );
}
     *)
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
                    (*| New of allocation_strategy option * typ * expression list*)
                    New (
                        Some Arena,
                        List (Class_type ("Point", Boehm)),
                        [List_init (List (Class_type ("Point", Boehm)))]
                    );
                );
            ];
            function_type = Function_type {return_type = Void; arguments = []}
        }
    ])

let%test_unit "trivial list infer class and allocation strat to C" =
    let code = [
        Ast.Function {
            name = "foo";
            docblock = [];
            params = [];
            stmts = [
                Assignment (
                    List (Class_type ("Point", Boehm)),
                    Variable "list",
                    (*| New of allocation_strategy option * typ * expression list*)
                    New (
                        Some Arena,
                        List (Class_type ("Point", Boehm)),
                        [List_init (List (Class_type ("Point", Boehm)))]
                    );
                );
            ];
            function_type = Function_type {return_type = Void; arguments = []}
        }
    ]
         |> Transpile.declarations_to_pholyglot
         |> Pholyglot_ast.string_of_declares
    in
    [%test_eq: Base.string] code {|#define function void
function foo()
#undef function
{
    #__C__ SplDoublyLinkedList
    $list = new(SplDoublyLinkedList
#__C__, arena_mem
);
    }
|}

let%test_unit "list memory context" =
    let source = {|<?php // @pholyglot
    function addItem(SplDoublyLinkedList $list): void {
        $p = /** @alloc $list */ new Point();
    }
    |} in
    let linebuf = Lexing.from_string source in
    let ns = Namespace.create () in
    let ast = Parser.program Lexer.token linebuf |> Infer.run ns in
    [%test_eq: Ast.program] ast (Declaration_list [])

(**
TODO:
    Conflicting @var
    Menhir explan:
        /home/olle/.opam/default/bin/menhir lib/parser.mly --base lib/parser --explain
 *)
