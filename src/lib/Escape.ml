open Ast

type escape_status =
    | Safe
    | Not_safe

let run : (program -> escape_status list) = function
    | Declaration_list decls ->
        (* Loop through all functions and statements to create namespace bindings *)
        let iter_decl = function
            | Function (name, params, stmts, typ) as fn ->
                let namespace = Namespace.create () in
                Namespace.add_assignments namespace fn;
                Safe
        in
        List.map iter_decl decls
