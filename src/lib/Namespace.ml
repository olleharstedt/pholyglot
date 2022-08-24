open Printf
open Ast

type t = (string, Ast.typ) Hashtbl.t

exception Namespace_error of string

let create () : t = Hashtbl.create 10

(**
 * Add value to t with key
 *)
let add t key value : unit =
    if Hashtbl.mem t key then
        raise (Namespace_error (sprintf "Key %s already exists in namespace" key))
    else
        Hashtbl.add t key value

let add_assignments t func = match func with
    | Function (name, params, stmts, typ) ->
        List.iter (fun (Param (id, v)) -> add t id v) params;
        let iter_stmt = function
            | Assignment (typ, identifier, expression) -> add t identifier typ
        in
        ()

