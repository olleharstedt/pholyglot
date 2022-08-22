(**
 * AST for Pholly
 *)

open Base

type program = 
    | Declaration_list of declaration list
[@@deriving show, compare, sexp]

(** TODO: Differ between value type and reference type? Always pass by reference except primitive types (int, string) *)
and typ =
    | Int
    | String
    | Struct_typ of struct_name
    | Mixed
    | Fixed_array of typ
    | Dynamic_array of typ
    | Tuple of typ              (* Example: Fixed_array (Tuple Int) *)
    | Linked_list
    | Hash_table
    | Infer_me
    | Void

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
and index = int

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
    | Function_call of typ * identifier * expression list
    (* If-statement, or only if-expression *)
    (* While-loop *)

and struct_init = (struct_field * expression) list

and expression =
    | Num of int
    (** TODO: Actually string literal *)
    | String of string
    | Plus of expression * expression
    | Minus of expression * expression
    | Times of expression * expression
    | Div of expression * expression
    | Concat of expression * expression
    | New of typ * expression list
    | Variable of identifier
    | Array_init of typ * expression list
    | Array_access of identifier * expression
    | Function_call of typ * identifier * expression list

    (*
    | Struct_access of expression * expression
    | Function_call of ...
    *)

let string_of_typ (t : typ) : string = match t with
    | Int -> "Int"
    | String -> "String"
    | Struct_typ _  -> "Struct_typ"
    | Infer_me -> "Infer_me"
    | Mixed -> "Mixed"
