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

and typ =
    | Int
    | String                (* Actually GString pointer *)
    | String_literal
    | Void
    | Var_args
    | Fixed_array of typ * int
    | Function_type of typ * typ list
    | Class_type of class_name
    | Infer_me

and param =
    | Param of identifier * typ

and declaration =
    | Function of function_name * param list * statement list * typ
    | Class of class_name * class_property list

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
    | Infer_me -> failwith "Cannot convert Infer_me to a C type"

(** Type notation that goes AFTER the variable name, as in array init *)
let string_of_typ_post = function
    | Fixed_array (t, n) -> sprintf {|
    #__C__ [%d]|} n
    | _ -> ""

let string_of_param (p: param) : string = match p with
    | Param (id, t) -> string_of_typ t ^ " " ^ id

let rec string_of_lvalue (l : lvalue) : string = match l with
    | Variable id -> id

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
    | Variable id -> "$" ^ id
    | Function_call _ -> failwith "string_of_expression: Not implemented: Function_call"
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

let string_of_statement = function
    | Return exp -> "return " ^ string_of_expression exp ^ ";\n"
    | Function_call (Function_type (Void, arg_types), id, exprs) ->
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
    | Assignment (typ, lvalue, expr) -> sprintf {|#__C__ %s
    $%s %s
    = %s;
    |}
    (string_of_typ typ)
    (string_of_lvalue lvalue)
    (string_of_typ_post typ)
    (string_of_expression expr)

let string_of_declare (d : declaration) : string = match d with
    | Function (name, params, stmts, typ) ->
        sprintf {|#__C__ %s
function %s(%s)
{
    %s}
|}
        (string_of_typ typ)
        name
        (concat ~sep:", " (List.map params ~f:string_of_param))
        (concat (List.map stmts ~f:string_of_statement))

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
