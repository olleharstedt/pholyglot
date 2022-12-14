open Printf
open Ast

type t = {
    (* Variables and functions go in identifiers bucket *)
    identifiers : (string, typ) Hashtbl.t;
    (* In PHP, you can have property and method with same name, so we need a triple instead of truple here *)
    classes : (string, (kind * class_property list * function_def list)) Hashtbl.t;
    functions: (string, typ) Hashtbl.t;
}

exception Namespace_error of string

let create () : t = {
    identifiers = Hashtbl.create 10;
    classes = Hashtbl.create 10;
    functions = Hashtbl.create 10;
}

(** Add value to namespace with key *)
let add_identifier t key value : unit =
    if Hashtbl.mem t.identifiers key then
        raise (Namespace_error (sprintf "add_identifier: Key '%s' already exists in identifiers namespace" key))
    else begin
        (*print_endline ("adding identifier " ^ key);*)
        Hashtbl.add t.identifiers key value
    end

let add_param t param : unit = 
    match param with
    | Param (id, typ) | RefParam (id, typ) -> add_identifier t id typ

let add_params t params : unit = List.iter (fun p -> add_param t p) params

let add_class_type t (c : Ast.declaration) = match c with
    | Class {name; kind; properties = props; methods} ->
        if Hashtbl.mem t.classes name then
            raise (Namespace_error (sprintf "add_class_type: Class name '%s' already exists in classes namespace" name))
        else
        Hashtbl.add t.classes name (kind, props, methods)

let add_function_type t name (typ : Ast.typ) =
    if Hashtbl.mem t.functions name then
        raise (Namespace_error (sprintf "add_function_type: Function name '%s' already exists in functions namespace" name))
    else
        Hashtbl.add t.functions name typ

(** As add_function_type but does nothing if the function already exists *)
let add_function_type_ignore t name (typ : Ast.typ) =
    if Hashtbl.mem t.functions name then ()
    else Hashtbl.add t.functions name typ

let find_identifier t key : typ option =
    Hashtbl.find_opt t.identifiers key

let find_class t id : (kind * class_property list * function_def list) option = 
    Hashtbl.find_opt t.classes id

let find_function t id : typ option =
    Hashtbl.find_opt t.functions id

(** Add variable assignments info namespace *)
let add_assignments t func = match func with
    | Function {
        name;
        docblock;
        params;
        stmts;
        function_type;
    } ->
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
    (* TODO: Add another populate for global lib functions? *)
    add_function_type_ignore t "printf" (Function_type {return_type = Void; arguments = [String_literal]});
    (*add_function_type_ignore t "array_slice" (Function_type {return_type = Dynamic_array (Type_variable "a"); arguments = [Fixed_array (Type_variable "a", None); Int]});*)
    add_function_type_ignore t "array_slice" (Function_type {return_type = Dynamic_array (Class_type "Body"); arguments = [Fixed_array (Class_type "Body", Some 3); Int]});
    t

(* Call this before passing namespace to another function to resetthe local namespace while keeping classes and functions types *)
let reset_identifiers t =
    {t with identifiers = (Hashtbl.create 10)} |> populate
