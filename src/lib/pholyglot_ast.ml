(* This is the AST for the PHP+C polyglot code *)

open Ppx_compare_lib.Builtin
open Base

type program = 
    start_line * 
    includes list * 
    defines list * 
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

and defines =
    | Define of identifier * string option

let string_of_start_line (s :start_line) : string = {|//<?php echo "\x08\x08"; ob_start(); ?>|}

let string_of_program (p : program) : string = match p with
    | (s, is, ds, decs, e) ->
        String.concat
        [
            string_of_start_line s
        ]
