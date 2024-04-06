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

let%test_unit "trivial arrayobject C" =
    let fn = Ast.Function {
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
        }
    in
    let phast = Transpile.declaration_to_pholyglot fn in
    let pholyglot_code = Pholyglot_ast.string_of_declare phast in
    [%test_eq: Base.string] pholyglot_code {|#define function int
function main()
#undef function
{
    #__C__ GC_INIT();
#__C__ ArrayObject
    $hash = new(ArrayObject
#__C__, gc_mem
);
    return 0;
}
|}

let%test_unit "foreach arrayobject" =
    Log.set_log_level Log.DEBUG;
    (* Location becomes ./_build/default/lib/arrayobject1.txt *)
    Log.set_output (open_out "arrayobject1.txt");
    let source = {|<?php // @pholyglot
    function main(): int {
        /** @var array<string, int> */
        $hash = new ArrayObject();
        $hash["Hello"] = "Hello";
        return 0;
    }
    |}
    (* 
       TODO
       $hash[10] = "Hello";
       *)
    in
    let linebuf = Lexing.from_string source in
    let ast = Parser.program Lexer.token linebuf in
    let ns = Namespace.create () in
    let ast = Infer.run ns ast in
    Log.set_log_level Log.FATAL;
    Log.clear_prefix ();
    Log.debug "should not be visible";
    [%test_eq: Ast.program] ast (Declaration_list [
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
                Hash_set {
                    hash_var = Variable "hash";
                    hash_typ = Hash_table (String, Int);
                    key = Num 10;
                    value = String "\"Hello\"";
                };
                Return (Num 0);
            ];
            function_type = Function_type {return_type = Int; arguments = []; uses_arena = false}
        })
    ])


(* TODO
 * alloc type
 * element alloc type?
 * hash table and value must use same mem alloc strat
 * hash table iterator
 *)
