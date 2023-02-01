open Printf
open Ast
module Log = Dolog.Log

let rec infer_stmt (s : statement) (ns : Namespace.t) : statement = 
    match s with
    | Assignment (Class_type (s, Infer_allocation_strategy), lvalue, expression) as a ->
        (* TODO: Figure out allocation strategy based on expression *)
        a
    | s -> s

let infer_declaration (decl : Ast.declaration) (ns : Namespace.t) : declaration = 
    match decl with
    | Function {name; docblock; params; stmts; function_type = Function_type {return_type; arguments}} as f ->
        let inf = fun s -> infer_stmt s ns in
        let new_stmts = List.map inf stmts in
        Function {name; docblock; params; stmts = new_stmts; function_type = Function_type {return_type; arguments}}
    | Class {name; kind; properties = props; methods} as c ->
        c

(* Infer.run must be run before this *)
let run (ns : Namespace.t) (p : program): program = 
    match p with
    | Declaration_list decls -> Declaration_list (List.map (fun d -> infer_declaration d ns) decls)
