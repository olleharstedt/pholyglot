open OUnit2
open Pholyglot.Ast
open Pholyglot

let tests = "assignment new" >::: [
    "mo" >:: (fun _ ->
        let ns = Namespace.create () in
        let ast = Infer.infer_stmt (Assignment (Infer_me, Variable "p", (New (Class_type ("Point", Infer_allocation_strategy), [])))) ns in
        assert_equal
            (Assignment (Class_type ("Point", Boehm), Variable "p", (New (Class_type ("Point", Boehm), []))))
            ast
        ~printer:Ast.show_statement
    );
]

let _ = run_test_tt_main tests
