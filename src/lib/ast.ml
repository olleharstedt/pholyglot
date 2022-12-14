(**
 * AST for Pholly
 *)

open Base

exception Parser_exception of string

type program = 
    | Declaration_list of declaration list
[@@deriving show, compare, sexp]

(* TODO: Differ between value type and reference type? Always pass by reference except primitive types (int, string) *)
and kind =
    | Ref
    | Val
    | Infer_kind

(* TODO: Add alloc type? Heap vs stack vs pool/region *)
and typ =
    | Type_variable of string
    | Int
    | Float
    (*| GChar Static glib string*)
    (*| GString Glib string buffer *)
    | String
    | String_literal            (* For library code *)
    | Constant
    | Class_type of class_name
    (* Fixed array can have Infer_me * None, when size is not yet known *)
    | Fixed_array of typ * int option
    | Dynamic_array of typ
    (*| Tuple of typ              (* Example: Fixed_array (Tuple Int) *) *)
    (*| Linked_list*)
    (*| Hash_table*)
    | Infer_me
    (* TODO: Infer_me of string option ? *)
    | Infer_class_name of string
    | Function_type of {
        return_type: typ;
        arguments: typ list;
    }
    | Void

and docblock_comment =
    | DocParam of identifier * typ

(* Function argument types *)
and param =
    | Param of identifier * typ
    (* For params with &$foo notation. Should only be allowed by arrays. *)
    | RefParam of identifier * typ

and function_def = {
        name:           function_name;
        docblock:       docblock_comment list;
        params:         param list;
        stmts:          statement list;
        function_type:  typ
    }

(* reduce/reduce conflict hack regarding "public" keyword *)
and class_elements =
    | Property of class_property
    | Method of function_def

(** Top-level constructs *)
and declaration =
    | Function of function_def
    | Class of {
        name: string; 
        kind: kind; 
        properties: class_property list;
        methods: function_def list;
    }

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
    | Plusplus of lvalue
    | Minusminus of lvalue
    | Function_call of typ * identifier * expression list
    | Foreach of {
        arr:       expression; (* Must be Variable expression *)
        key:       expression option; (* Must be Variable if it exists *)
        value:     expression; (* Must be Variable *)
        value_typ: typ;
        value_typ_constant: expression;
        body:      statement list;
    }
    (* If-statement, or only if-expression *)
    (* While-loop *)

and class_init = (class_property * expression) list

(* What's allowed on left side of assignment *)
and lvalue =
    (* TODO: Should be able to build Object_access id, Variable *)
    | Variable of identifier
    | Property_access of class_property_name
    | Object_access of identifier * lvalue

and expression =
    | Nil (* Empty expression for internal use in foreach *)
    | Num of int
    | Num_float of float
    (** TODO: GString *)
    | String of string
    | Constant of string
    | Plus of expression * expression
    | Minus of expression * expression
    | Times of expression * expression
    | Div of expression * expression
    | Concat of expression * expression
    | Lessthan of expression * expression
    | Greaterthan of expression * expression
    | New of typ * expression list
    | Variable of identifier
    | Array_init of typ * int option * expression list
    | Array_access of identifier * expression
    (*
     * Examples:
     *   $obj->x    - Object_access (Variable "obj", Property_access "x")
     *   $arr[0]->x - Object_access (Array_access ("arr", Num 0), Property_access "x")
     *
     * "expression" can be property, method, or nested object access
     *)
    | Object_access of expression * expression
    | Property_access of identifier (* Valid sub-expression of object access *)
    | Method_call of {
        return_type: typ;
        method_name: string;
        left_hand: expression; (* left side of arrow, $obj in $obj->foo() *)
        args: expression list;
    }
    | Function_call of typ * identifier * expression list
    | Coerce of typ * expression

let get_arg_types_from_args args =
    let f : param -> typ = fun (Param (_, t) | RefParam (_, t)) -> t in
    List.map ~f:f args

(* Returns true if typ is "value type" and allowed to escape/be copied automatically by C *)
let typ_is_val = function
    | Int -> true
    | _ -> false
