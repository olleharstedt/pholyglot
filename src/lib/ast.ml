(**
 * AST for Pholly
 *)

open Base

type program = 
    | Declaration_list of declaration list
[@@deriving show, compare, sexp]

(* TODO: Differ between value type and reference type? Always pass by reference except primitive types (int, string) *)
(* TODO: Add alloc type? Heap vs stack vs pool/region *)
and typ =
    | Int
    (*| GChar Static glib string*)
    (*| GString Glib string buffer *)
    | String
    | String_literal            (* For library code *)
    | Class_type of class_name
    (*| Mixed*)
    | Fixed_array of typ * int
    (*| Dynamic_array of typ*)
    (*| Tuple of typ              (* Example: Fixed_array (Tuple Int) *) *)
    (*| Linked_list*)
    (*| Hash_table*)
    | Infer_me
    | Function_type of typ * typ list
    | Void

and param =
    | Param of identifier * typ

(** Top-level constructs *)
and declaration =
    | Function of function_name * param list * statement list * typ
    | Class of class_name * class_property list

and function_name = string
and class_name = string
and class_property_name = string
and class_property = class_property_name * typ
and identifier = string
and region_name = string
and index = int

and statement =
    (* Struct_alloc is an internal statement used by C pass *)
    (*| Struct_alloc of typ * identifier * struct_init*)
    (* Internal statement used by region pass *)
    (*| Struct_pool_alloc of region_name * typ * identifier * struct_init*)
    (* let a = ...; *)
    (* TODO: $a->b->c = ... *)
    | Assignment of typ * lvalue * expression
    (* return ...; *)
    | Return of expression
    | Function_call of typ * identifier * expression list
    (* If-statement, or only if-expression *)
    (* While-loop *)

and class_init = (class_property * expression) list

(* What's allowed on left side of assignment *)
and lvalue =
    | Identifier of identifier
    | Object_access of identifier * lvalue

and expression =
    | Num of int
    (** TODO: GString *)
    | String of string
    | Plus of expression * expression
    | Minus of expression * expression
    | Times of expression * expression
    | Div of expression * expression
    | Concat of expression * expression
    | New of typ * expression list
    | Variable of identifier
    | Array_init of expression list
    | Array_access of identifier * expression
    | Object_access of identifier * expression
    | Identifier of identifier (* Valid sub-expression of object access *)
    | Function_call of typ * identifier * expression list
    | Coerce of typ * expression
