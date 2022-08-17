(**
 * AST for Pholly
 *)

open Ppx_compare_lib.Builtin
open Base

type program = 
    | Declaration_list of declaration list
[@@deriving show, compare, sexp]

(** TODO: Differ between value type and reference type? Always pass by reference except primitive types (int, string) *)
and typ =
    | Int
    | Struct_typ of struct_name
    | Infer_me

and param =
    | Param of identifier * typ

(** Top-level constructs *)
and declaration =
    | Function of function_name * param list * statement list * typ
    | Struct of struct_name * struct_field list

and function_name = string

and struct_name = string

and struct_field_name = string

and struct_field = struct_field_name * typ

and identifier = string

and region_name = string

and statement =
    (* Struct_alloc is an internal statement used by C pass *)
    | Struct_alloc of typ * identifier * struct_init
    (* Internal statement used by region pass *)
    | Struct_pool_alloc of region_name * typ * identifier * struct_init
    (* let a = ...; *)
    | Assignment of typ * identifier * expression
    (* return ...; *)
    | Return of expression
    | New_region of region_name
    (* Function_call that returns void *)
    (* If-statement, or only if-expression *)
    (* While-loop *)

and struct_init = (struct_field * expression) list

and expression =
    | Num of int
    | Plus of expression * expression
    | New of typ * expression list
    | Variable of identifier
    (*
    | Struct_access of expression * expression
    | Function_call of ...
    *)

let type_of_string (s : string) : typ = match s with
    | s -> Struct_typ s

let string_of_typ (t : typ) : string = match t with
    | Int -> "Int"
    | Struct_typ _  -> "Struct_typ"
    | Infer_me -> "Infer_me"
