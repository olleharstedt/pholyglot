{

open Lexing
open Docblockparser

exception DocblockSyntaxError of string

}

let digit = ['0'-'9']
let nondigit = ['_' 'a'-'z' 'A'-'Z']
let uppercase = ['A'-'Z']
let lowercase = ['a'-'z']
let nonzero_digit = ['1'-'9']
let decimal_constant = nonzero_digit digit*
let identifier = lowercase (nondigit|digit)*
let whitespace_char_no_newline = [' ' '\t' '\012' '\r']

rule docblock = parse
    (*
  | class_name as n     { CLASS_NAME n}
  (* TODO: Type regexp, like array<int>, so alphanumeric + <> + comma *)
  (* TODO: Class name with capital letter *)
  (* TODO: array<Point> *)
  (* TODO: array<string, int> *)
  (* TODO: Variable name, $moo, so $ + alphanumeric and underscore *)
  | ""                  { docblock lexbuf }
  *)
  | "$"                 { DOLLAR }
  | "@param"            { DOCBLOCK_PARAM }
  | "array"             { ARRAY_TYPE }
  | "int"               { INT_TYPE }
  | "<"                 { LT }
  | ">"                 { GT }
  | ","                 { COMMA }
  | identifier as id    { NAME id }
  | whitespace_char_no_newline+   { docblock lexbuf }
  | '\n'                { new_line lexbuf; docblock lexbuf }
  | "/"                { EOF }
  | eof                 { failwith "unterminated comment" }
  | _                   { raise (DocblockSyntaxError ("Docblocklexer - Illegal character: " ^ Lexing.lexeme lexbuf)) }
