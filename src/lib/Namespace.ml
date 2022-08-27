open Printf
open Ast

type t = (string, typ) Hashtbl.t

exception Namespace_error of string

let create () : t = Hashtbl.create 10

(** Add value to namespace with key *)
let add t key value : unit =
    if Hashtbl.mem t key then
        raise (Namespace_error (sprintf "Key '%s' already exists in namespace" key))
    else
        Hashtbl.add t key value

let find t key : typ option =
    Hashtbl.find_opt t key

(** Add variable assignments info namespace *)
let add_assignments t func = match func with
    | Function (name, params, stmts, typ) ->
        List.iter (fun (Param (id, v)) -> add t id v) params;
        let iter_stmt = function
            | Assignment (typ, identifier, expression) -> add t identifier typ
            | _ -> () (* TODO: Assignments inside if-statements etc *)
        in
        List.iter iter_stmt stmts

(**
 * Populate namespace with global library functions
 * Return namespace to do a pipe
 *)
let populate t : t=
    add t "printf" (Function_type (Void, [String_literal]));
    t
