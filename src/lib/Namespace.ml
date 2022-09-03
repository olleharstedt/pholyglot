open Printf
open Ast

type t = {
    (* Variables and functions go in identifiers bucket *)
    identifiers : (string, typ) Hashtbl.t;
    classes : (string, class_property list) Hashtbl.t;
    functions: (string, typ) Hashtbl.t;
}

exception Namespace_error of string

let create () : t = {
    identifiers = Hashtbl.create 10;
    classes = Hashtbl.create 10;
}

(** Add value to namespace with key *)
let add_identifier t key value : unit =
    if Hashtbl.mem t.identifiers key then
        raise (Namespace_error (sprintf "add_identifier: Key '%s' already exists in identifiers namespace" key))
    else
        Hashtbl.add t.identifiers key value

let add_param t param : unit = 
    match param with
    | Param (id, typ) -> add_identifier t id typ

let add_params t params : unit = List.iter (fun p -> add_param t p) params

let add_class_type t (c : Ast.declaration) = match c with
    | Class (name,  props) ->
        if Hashtbl.mem t.classes name then
            raise (Namespace_error (sprintf "add_class_type: Class name '%s' already exists in classes namespace" name))
        else
        Hashtbl.add t.classes name props

let find_identifier t key : typ option =
    Hashtbl.find_opt t.identifiers key

let find_class t id : class_property list option = 
    Hashtbl.find_opt t.classes id

(** Add variable assignments info namespace *)
let add_assignments t func = match func with
    | Function (name, params, stmts, typ) ->
        List.iter (fun (Param (id, v)) -> add_identifier t id v) params;
        let iter_stmt = function
            | Assignment (typ, Variable id, expression) -> add_identifier t id typ
            | _ -> () (* TODO: Assignments inside if-statements etc *)
        in
        List.iter iter_stmt stmts

(**
 * Populate namespace with global library functions
 * Return namespace to do a pipe
 *)
let populate (t : t) : t=
    add_identifier t "printf" (Function_type (Void, [String_literal]));
    t

(* Call this before passing namespace to another function *)
let reset_identifiers t =
    {t with identifiers = (Hashtbl.create 10)} |> populate
