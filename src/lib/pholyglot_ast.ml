(* This is the AST for the PHP+C polyglot code *)

open Base

type program = 
    start_line * 
    includes list * 
    define list * 
    declaration list * 
    end_line
[@@deriving show, compare, sexp]

and start_line = Start_line
and end_line = End_line

and typ =
    | Int
    | String
    | Infer_me

and param =
    | Param of identifier * typ

and declaration =
    | Function of function_name * param list * statement list * typ

and function_name = string
and identifier = string
and include_lib = string

and statement =
    | Return of expression

and expression =
    | Num of int

and includes =
    | Include of include_lib

and define =
    | Define of identifier * string option

let string_of_start_line = function Start_line -> {|//<?php echo "\x08\x08"; ob_start(); ?>
|}


let string_of_include = function
    | Include l -> "#include " ^ l

let string_of_define (d : define) : string = match d with
    | Define (id, Some s) -> Printf.sprintf "#define %s %s\n" id s
    | Define (id, None) -> Printf.sprintf "#define %s \n" id

let string_of_typ (t : typ) : string = match t with
    | Int -> "int"
    | _ -> failwith "string_of_typ"

let string_of_param (p: param) : string = match p with
    | Param (id, t) -> string_of_typ t ^ " " ^ id

let string_of_expression = function
    | Num i -> Int.to_string i

let string_of_statement = function
    | Return exp -> "return " ^ string_of_expression exp ^ ";\n"

let string_of_declare (d : declaration) : string = match d with
    | Function (name, params, stmts, typ) ->
        Printf.sprintf {|#__C__ %s
function %s(%s)
{
    %s}
|}
        (string_of_typ typ)
        name
        (String.concat ~sep:", " (List.map params ~f:string_of_param))
        (String.concat (List.map stmts ~f:string_of_statement))

let string_of_end_line = function End_line -> {|// ?>
// <?php ob_end_clean(); main();|}

let string_of_program (p : program) : string = match p with
    | (s, is, ds, decs, e) ->
        String.concat
        [
            string_of_start_line s;
            String.concat (List.map is ~f:string_of_include);
            String.concat (List.map ds ~f:string_of_define);
            "//<?php\n";
            String.concat (List.map decs ~f:string_of_declare);
            string_of_end_line e
        ]
