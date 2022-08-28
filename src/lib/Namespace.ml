open Printf
open Ast

type t = {
    (* Variables and functions go in identifiers bucket *)
    identifiers : (string, typ) Hashtbl.t;
    classes : (string, typ) Hashtbl.t;
}

exception Namespace_error of string

let create () : t = {
    identifiers = Hashtbl.create 10;
    classes = Hashtbl.create 10;
}

(** Add value to namespace with key *)
let add_identifier t key value : unit =
    if Hashtbl.mem t.identifiers key then
        raise (Namespace_error (sprintf "Key '%s' already exists in namespace" key))
    else
        Hashtbl.add t.identifiers key value

let find_identifier t key : typ option =
    Hashtbl.find_opt t.identifiers key

(** Add variable assignments info namespace *)
let add_assignments t func = match func with
    | Function (name, params, stmts, typ) ->
        List.iter (fun (Param (id, v)) -> add_identifier t id v) params;
        let iter_stmt = function
            | Assignment (typ, Variable id, expression) -> add_identifier t id typ
            | _ -> () (* TODO: Assignments inside if-statements etc *)
        in
        List.iter iter_stmt stmts

let add_class_type t c = match c with
    | Class (name,  props) -> ()

(**
 * Populate namespace with global library functions
 * Return namespace to do a pipe
 *)
let populate (t : t) : t=
    add_identifier t "printf" (Function_type (Void, [String_literal]));
    t
