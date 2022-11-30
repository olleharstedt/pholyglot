(* This is the AST for the PHP+C polyglot code *)

open Base
open Printf
open String

type program = 
    start_line * 
    includes list * 
    define list * 
    php_stubs list * 
    declaration list * 
    end_line
[@@deriving show, compare, sexp]

and start_line = Start_line
and end_line = End_line

and kind =
    | Ref
    | Val

and typ =
    | Int
    | String                (* Actually GString pointer *)
    | String_literal
    | Void
    | Var_args
    | Fixed_array of typ * int
    | Function_type of {return_type: typ; arguments: typ list}
    | Class_type of class_name

and param =
    | Param of identifier * typ

and docblock_comment =
    | DocParam of identifier * typ

and function_def = {
        name:           function_name;
        params:         param list;
        stmts:          statement list;
        function_type:  typ
    }

and declaration =
    | Function of function_def
    | Class of class_name * kind * class_property list * function_def list

and function_name = string
and class_name = string
and identifier = string
and include_lib = string
and php_stubs = string
and class_property_name = string
and class_property = class_property_name * typ

and statement =
    | Return of expression
    | Assignment of typ * lvalue * expression
    | Function_call of typ * identifier * expression list

and lvalue =
    | Variable of identifier
    | Property_access of class_property_name
    | Object_access of identifier * lvalue

and expression =
    | Num of int
    | String of string
    | Plus of expression * expression
    | Minus of expression * expression
    | Times of expression * expression
    | Div of expression * expression
    | Concat of expression * expression
    | Variable of identifier
    | Array_init of expression list
    | Array_access of identifier * expression
    | Object_access of identifier * expression
    | New of typ * expression list
    | Property_access of identifier (* Valid sub-expression of object access *)
    | Method_call of {
        return_type: typ;
        method_name: string;
        object_name: string;
        args: expression list;
    }
    | Function_call of typ * identifier * expression list
    | Coerce of typ * expression

and includes =
    | Include of include_lib

and define =
    | Define of identifier * string option

let string_of_start_line = function Start_line -> {|//<?php echo "\x08\x08"; ob_start(); ?>
|}


let string_of_include = function
    | Include l -> sprintf "#include <%s>\n" l

let string_of_define (d : define) : string = match d with
    | Define (id, Some s) -> sprintf "#define %s %s\n" id s
    | Define (id, None) -> sprintf "#define %s \n" id

let rec string_of_typ (t : typ) : string = match t with
    | Int -> "int"
    | String -> "GString*"
    | Void -> "void"
    | Fixed_array (t, n) -> string_of_typ t
    (* Assuming we have a proper typedef, this is OK in both PHP and C *)
    | Class_type (n) -> n
    | Function_type {return_type; arguments} -> string_of_typ return_type

(** Type notation that goes AFTER the variable name, as in array init *)
let string_of_typ_post = function
    | Fixed_array (t, n) -> sprintf {|
    #__C__ [%d]|} n
    | _ -> ""

let string_of_param (p: param) : string = match p with
    | Param (id, t) -> string_of_typ t ^ " $" ^ id

let rec string_of_lvalue (l : lvalue) : string = match l with
    | Variable id -> id
    | Object_access (id, Property_access prop_name) ->
        (* TODO Code duplication *)
        let id = if id = "this" then "self" else id in
        sprintf {|%s->%s|} id prop_name
    | Property_access n -> n

let rec string_of_expression = function
    | Num i -> Int.to_string i
	(* TODO: Problem with GString vs string for expressions, append vs use as function arg *)
    | String s -> sprintf "g_string_new(%s)" s
    | Coerce (String_literal, String s) -> s
    | Coerce (_, _) -> failwith "string_of_expression: Coerce: Do not know what to coerce"
    | Plus (i, j) -> (string_of_expression i) ^ " + " ^ (string_of_expression j)
    | Minus (i, j) -> (string_of_expression i) ^ " - " ^ (string_of_expression j)
    | Times (i, j) -> (string_of_expression i) ^ " * " ^ (string_of_expression j)
    | Div (i, j) -> (string_of_expression i) ^ " / " ^ (string_of_expression j)
    | Concat (s, t) -> sprintf "g_string_append(%s, %s->str)" (string_of_expression s) (string_of_expression t)
    | Variable id ->
        let id = if id = "this" then "self" else id in
        "$" ^ id
    (* TODO: Alloc type *)
    (* TODO: Init function pointers *)
    | New (Class_type (ct), exprs) -> 
        (*let t_text = show_typ t in*)
        sprintf {|new_(%s)|}
        ct
    | Array_init exprs -> sprintf {|
#__C__ {
#if __PHP__
[
#endif
    %s
#if __PHP__
]
#endif
#__C__ }
    |}
        (concat ~sep:", " (List.map exprs ~f:string_of_expression))
    | Array_access (id, expr) ->
        (* TODO: It's assumed that expr has type Int here *)
        sprintf {|$%s[%s]|} id (string_of_expression expr)
    | Object_access (id, Property_access prop_name) ->
        (* TODO: Code duplication *)
        let id = if id = "this" then "self" else id in
        sprintf {|$%s->%s|} id prop_name
    | Object_access (id, Method_call {return_type; method_name; object_name; args}) ->
        sprintf {|$%s->%s(%s)|}
        object_name
        method_name 
        (concat ~sep:", " (List.map args ~f:string_of_expression))
    | Property_access n -> n
    | Function_call (Function_type {return_type; arguments}, name, param_exprs) ->
        sprintf
        {|%s(%s)|}
        name
        (concat ~sep:", " (List.map param_exprs ~f:string_of_expression))
    (*| Method_call {return_type; method_name; object_name; args} ->*)
    | e -> failwith ("string_of_expression: " ^ show_expression e)

let string_of_statement = function
    | Return exp -> "return " ^ string_of_expression exp ^ ";\n"
    | Function_call (Function_type {return_type = Void; arguments}, id, exprs) ->
        sprintf {| %s(%s);
    |}
        id
        (concat ~sep:", " (List.map exprs ~f:string_of_expression))
    | Function_call (fun_type, id, _) ->
        failwith (
            sprintf
            "string_of_statement: Function_call: Can only call functions that return void as statements; %s does not: %s"
            id
            (string_of_typ fun_type)
        )
    | Assignment (typ, Variable v, expr) -> sprintf {|#__C__ %s
    $%s %s
    = %s;
    |}
    (string_of_typ typ)
    v
    (string_of_typ_post typ)
    (string_of_expression expr)
    | Assignment (typ, lvalue, expr) -> sprintf {|$%s %s
    = %s;
    |}
    (string_of_lvalue lvalue)
    (string_of_typ_post typ)
    (string_of_expression expr)


let string_of_prop (p : class_property) : string = match p with
    | (n, t) -> sprintf {|#define public %s
#define %s $%s
    public $%s;
#undef public
|}
    (string_of_typ t)
    n
    n
    n

(* Function pointers are defined BEFORE the typedef, so 'struct' needs to be written explicitly for class types *)
(* #__C__ char* (*getName) (struct Point*); *)
let string_of_function_pointer meth : string = match meth with
  (*| (name, params, stmts, Function_type {return_type; arguments}) ->*)
    | { name; params; stmts; function_type = Function_type {return_type; arguments}} ->
    sprintf {|#__C__ %s (*%s) (%s);|}
    (* Return type *)
    (string_of_typ (Function_type {return_type; arguments}))
    (* Function name *)
    name
    (* Params *)
    (concat ~sep:", " (List.map params ~f:string_of_param))

(**
 * A method must be valid both as C function and PHP class method
 *
 * @param name : string Class name, used by self
 * @param meth : method
 * @return string
 *)
let string_of_method class_name meth = match meth with
    | { name; params; stmts; function_type = Function_type {return_type; arguments}} ->
        sprintf {|
#if __PHP__
public function %s(%s): %s
#endif
#__C__ %s %s_%s (%s)
{
    %s
}
|}
    (* PHP function name *)
    name
    (* Params for PHP *)
    (concat ~sep:", " (List.map params ~f:string_of_param))
    (* PHP return type *)
    (string_of_typ (Function_type {return_type; arguments}))
    (* C Return type *)
    (string_of_typ (Function_type {return_type; arguments}))
    class_name
    name
    (* Params for C *)
    (concat ~sep:", " (List.map params ~f:string_of_param))
    (concat (List.map stmts ~f:string_of_statement))

let string_of_function_pointer_init class_name meth = match meth with
    | { name; } ->
        sprintf {|$p->%s = &%s_%s;
|}
        name
        class_name
        name

let string_of_declare (d : declaration) : string = match d with
    | Function {
        name;
        params;
        stmts;
        function_type = typ
    } ->
        sprintf {|#define function %s
function %s(%s)
{
    %s}
#undef function
|}
        (string_of_typ typ)
        name
        (concat ~sep:", " (List.map params ~f:string_of_param))
        (concat (List.map stmts ~f:string_of_statement))
    | Class (class_name, kind, props, methods) ->
        let string_of_method = fun (m) -> string_of_method class_name m in
        let string_of_function_pointer_init = fun (m) -> string_of_function_pointer_init class_name m in
        sprintf {|
class %s {
    %s
    %s
// End of C struct def. Class methods are outside the struct.
#__C__ }; typedef struct %s* %s;
%s
#if __PHP__
// End of PHP class def.
};
#endif
#if __PHP__
define("%s", "%s");  // Needed to make new_() work with C macro
#endif
//?>
// Function pointer init
%s new_%s(%s $p)
{
    %s
    return $p;
}
//<?php
#if __PHP__
function new_%s($p) { return $p; }
#endif
|}
        class_name
        (concat (List.map ~f:string_of_prop props))
        (concat (List.map ~f:string_of_function_pointer methods))
        (concat (List.map ~f:string_of_method methods))
        class_name
        class_name
        class_name
        class_name
        class_name
        class_name
        class_name
        (concat (List.map ~f:string_of_function_pointer_init methods))
        class_name

(** Probably only to be used in tests *)
let string_of_declares ds : string = concat (List.map ds ~f:string_of_declare)

let string_of_end_line = function End_line -> {|// ?>
// <?php ob_end_clean(); main();|}

let string_of_program (p : program) : string = match p with
    | (s, is, ds, stubs, decs, e) ->
        concat
        [
            string_of_start_line s;
            concat (List.map is ~f:string_of_include);
            concat (List.map ds ~f:string_of_define);
            "#if __PHP__//<?php\n";
            concat stubs;
            "#endif//?>\n";
            "//<?php\n";
            concat (List.map decs ~f:string_of_declare);
            string_of_end_line e
        ]
