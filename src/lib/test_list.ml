module Log = Dolog.Log

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
    [%test_eq: Ast.program] ast (Declaration_list [
        Function {
            name = "addItem";
            docblock = [];
            params = [Param ("list", List Infer_me)];
            stmts = [
                Assignment (
                    Class_type ("Point", Boehm),
                    Variable "p",
                    New (
                        Some (Memory_context "list"),
                        Class_type ("Point", Memory_context "list"),
                        []
                    );
                );
            ];
            function_type = Function_type {return_type = Void; arguments = [List Infer_me]}
        }
    ])

let%test_unit "list memory context" =
    let source = {|<?php // @pholyglot
    /**
     * @param SplDoublyLinkedList<Point> $list
     */
    function addItem(SplDoublyLinkedList $list): void {
        $p = /** @alloc $list */ new Point();
    }
    |} in
    let linebuf = Lexing.from_string source in
    let ns = Namespace.create () in
    let ast = Parser.program Lexer.token linebuf |> Infer.run ns in
    [%test_eq: Ast.program] ast (Declaration_list [
        Function {
            name = "addItem";
            docblock = [DocParam ("list", List (Class_type ("Point", Memory_polymorph)))];
            params = [Param ("list", List (Class_type ("Point", Memory_polymorph)))];
            stmts = [
                Assignment (
                    Class_type ("Point", Boehm),
                    Variable "p",
                    New (
                        Some (Memory_context "list"),
                        Class_type ("Point", Memory_context "list"),
                        []
                    );
                );
            ];
            function_type = Function_type {return_type = Void; arguments = [List (Class_type ("Point", Memory_polymorph))]}
        }
    ])

let%test_unit "output list memory context" =
    let code = [
        Ast.Function {
            name = "addItem";
            docblock = [DocParam ("list", List (Class_type ("Point", Memory_polymorph)))];
            params = [Param ("list", List (Class_type ("Point", Memory_polymorph)))];
            stmts = [
                Assignment (
                    Class_type ("Point", Boehm),
                    Variable "p",
                    New (
                        Some (Memory_context "list"),
                        Class_type ("Point", Memory_context "list"),
                        []
                    );
                );
            ];
            function_type = Function_type {return_type = Void; arguments = [List (Class_type ("Point", Memory_polymorph))]}
        }
    ]
         |> Transpile.declarations_to_pholyglot
         |> Pholyglot_ast.string_of_declares
    in
    [%test_eq: Base.string] code {|#define function void
function addItem(SplDoublyLinkedList $list)
#undef function
{
    #__C__ Point
    $p = new(Point
#__C__, $list->mem
);
    }
|}

let%test_unit "add items" =
    let source = {|<?php // @pholyglot
class Point {
    public int $x;
}

/**
 * @param SplDoublyLinkedList<Point> $list
 */
function printlist(SplDoublyLinkedList $list): void
{
    foreach ($list as $item) {
        $x = $item->x;
    }
}
    |} in
    let linebuf = Lexing.from_string source in
    let ns = Namespace.create () in
    let ast = Parser.program Lexer.token linebuf |> Infer.run ns in
    (*[%test_eq: Base.string] code {||}*)
    [%test_eq: Ast.program] ast (Declaration_list [
        Class {
            name= "Point";
            kind = Val;
            properties = [
                ("__prop_x", Int);
            ];
            methods = [];
        };
        Function {
            name = "printlist";
            docblock = [DocParam ("list", List (Class_type ("Point", Memory_polymorph)))];
            params = [Param ("list", List (Class_type ("Point", Memory_polymorph)))];
            stmts = [
                Foreach_list {
                    arr = Variable "list";
                    key = None;
                    value = Variable "item";
                    value_typ = Class_type ("Point", Memory_polymorph);
                    body = [
                        Assignment (
                            Int,
                            Variable "x",
                            Object_access (Variable "item", Property_access "__prop_x");
                        );
                    ];
                };
            ];
            function_type = Function_type {return_type = Void; arguments = [List (Class_type ("Point", Memory_polymorph))]}
        }
    ])

let%test_unit "foreach list code" =
    let code = [
        Ast.Function {
            name = "printlist";
            docblock = [DocParam ("list", List (Class_type ("Point", Memory_polymorph)))];
            params = [Param ("list", List (Class_type ("Point", Memory_polymorph)))];
            stmts = [
                Foreach_list {
                    arr = Variable "list";
                    key = None;
                    value = Variable "item";
                    value_typ = Class_type ("Point", Memory_polymorph);
                    body = [
                        Assignment (
                            Int,
                            Variable "x",
                            Object_access (Variable "item", Property_access "__prop_x");
                        );
                    ];
                };
            ];
            function_type = Function_type {return_type = Void; arguments = [List (Class_type ("Point", Memory_polymorph))]}
        }
    ]
         |> Transpile.declarations_to_pholyglot
         |> Pholyglot_ast.string_of_declares
    in
    [%test_eq: Base.string] code {|#define function void
function printlist(SplDoublyLinkedList $list)
#undef function
{
    $list->rewind(
#__C__ $list
);
do {
#__C__ Point
    $item = $list->current(
#__C__ $list
);
    #__C__ int
    $x = $item->__prop_x;
    $list->next(
    #__C__ $list
);
} while ($list->valid(
#__C__ $list
));
}
|}

let%test_unit "push item" =
    let source = {|<?php // @pholyglot
/**
 * @param SplDoublyLinkedList<Point> $list
 * @param Point $p
 */
function additems(SplDoublyLinkedList $list, Point $p): void
{
    $list->push($p);
}
|}
    in
    let linebuf = Lexing.from_string source in
    let ns = Namespace.create () in
    let ast = Parser.program Lexer.token linebuf |> Infer.run ns in
    (*[%test_eq: Base.string] code {||}*)
    [%test_eq: Ast.program] ast (Declaration_list [
        Function {
            name = "additems";
            docblock = [
                DocParam ("list", List (Class_type ("Point", Memory_polymorph)));
                DocParam ("p", Class_type ("Point", Memory_polymorph));
            ];
            params = [
                Param ("list", List (Class_type ("Point", Memory_polymorph)));
                Param ("p", Class_type ("Point", Memory_polymorph));
            ];
            stmts = [
                Method_call {
                    lvalue   = Object_access ("list", Property_access "__prop_push");
                    lvalue_t = List (Class_type ("Point", Memory_polymorph));
                    args     = [Variable "p"];
                }
            ];
            function_type = Function_type {
                return_type = Void;
                arguments = [
                    List (Class_type ("Point", Memory_polymorph));
                    Class_type ("Point", Memory_polymorph);
                ];
            }
        }
    ])

(*
TODO:
    Include Arena in function init (only if arena is used?)
    Default to Boehm gc if none is given
    ArgumentCountError: SplDoublyLinkedList::push() expects exactly 1 argument, 2 given in /home/olle/kod/pholyglot/list/tmp.c:88
        Only happens with built-in libs, since I can't control function sig
    Conflicting @var
    Menhir explan:
        /home/olle/.opam/default/bin/menhir lib/parser.mly --base lib/parser --explain
    Memory_context can only be class or list, not basic types such as int or string
*)
