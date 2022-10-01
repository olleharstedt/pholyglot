(**************************************************************************)
(*                                                                        *)
(*  Menhir                                                                *)
(*                                                                        *)
(*  François Pottier, INRIA Rocquencourt                                  *)
(*  Yann Régis-Gianas, PPS, Université Paris Diderot                      *)
(*                                                                        *)
(*  Copyright 2005-2008 Institut National de Recherche en Informatique    *)
(*  et en Automatique. All rights reserved. This file is distributed      *)
(*  under the terms of the Q Public License version 1.0, with the change  *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(**************************************************************************)


{
open Lexing
open Parser

exception Error of string
exception SyntaxError of string
}

(** Copied from Jacques-Henri Jourdan, Inria Paris *)
(** https://github.com/jhjourdan/C11parser *)
let digit = ['0'-'9']
let nondigit = ['_' 'a'-'z' 'A'-'Z']
let uppercase = ['A'-'Z']
let lowercase = ['a'-'z']
let nonzero_digit = ['1'-'9']
let decimal_constant = nonzero_digit digit*
let identifier = lowercase (nondigit|digit)*
let class_name = uppercase (nondigit|digit)*
let whitespace_char_no_newline = [' ' '\t' '\012' '\r']
let integer_constant = decimal_constant

rule token = parse
  | whitespace_char_no_newline+   { token lexbuf }
  | "/*"                          { multiline_comment lexbuf; token lexbuf }
  | "/**"                         { docblock lexbuf }
  | "//"                          { singleline_comment lexbuf; initial_linebegin lexbuf }
  | '\n'                          { new_line lexbuf; initial_linebegin lexbuf }
  | integer_constant as i         { INT (int_of_string i) }
  | "<?php // @pholyglot"         { START_SCRIPT }
  | "0"                           { INT 0 }
  | "="                           { EQ }
  | "=="                          { EQEQ }
  | "==="                         { EQEQ }
  | "<"                           { LT }
  | ">"                           { GT }
  | ';'                           { SEMICOLON }
  | ':'                           { COLON }
  | '+'                           { PLUS }
  | '-'                           { MINUS }
  | '*'                           { TIMES }
  | '/'                           { DIV }
  | "{"                           { LBRACE }
  | "}"                           { RBRACE }
  | "["                           { LBRACK }
  | "]"                           { RBRACK }
  | '('                           { LPAREN }
  | ')'                           { RPAREN }
  | ','                           { COMMA }
  | '.'                           { DOT }
  | "->"                          { ARROW }
  | "return"                      { RETURN }
  | "new"                         { NEW }
  | "function"                    { FUNCTION }
  | "class"                       { CLASS }
  | "public"                      { PUBLIC }
  | "int"                         { INT_TYPE }
  | "void"                        { VOID_TYPE }
  | "string"                      { STRING_TYPE }
  | "array"                       { ARRAY_TYPE }
  | "$"                           { DOLLAR }
  | "&"                           { AMPERSAND }
  | ['"'] [^ '"']+ ['"'] as s     { STRING_LITERAL s }
  | class_name as n               { CLASS_NAME n}
  | identifier as id              { NAME id }
  | eof                           { EOF }
  | _ { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }

and initial_linebegin = parse
  | '\n'                          { new_line lexbuf; initial_linebegin lexbuf }
  | whitespace_char_no_newline    { initial_linebegin lexbuf }
  | ""                            { token lexbuf }

  (* Single-line comment terminated by a newline *)
and singleline_comment = parse
  | '\n'   { new_line lexbuf }
  | eof    { () }
  | _      { singleline_comment lexbuf }


  (* Multi-line comment terminated by "*/" *)
and multiline_comment = parse
  | "*/"   { () }
  | eof    { failwith "unterminated comment" }
  | '\n'   { new_line lexbuf; multiline_comment lexbuf }
  | _      { multiline_comment lexbuf }

and docblock = parse
  | whitespace_char_no_newline+   { docblock lexbuf }
  | "*"                 { docblock lexbuf }
  | ""                  { docblock lexbuf }
  | "@param"            { DOCBLOCK_PARAM }
  | "<"                 { LT }
  | ">"                 { GT }
  | ","                 { COMMA }
  | "$"                 { DOLLAR }
  | identifier as id    { NAME id }
  | class_name as n     { CLASS_NAME n}
  (* TODO: Type regexp, like array<int>, so alphanumeric + <> + comma *)
  (* TODO: Class name with capital letter *)
  (* TODO: array<Point> *)
  (* TODO: array<string, int> *)
  (* TODO: Variable name, $moo, so $ + alphanumeric and underscore *)
  | '\n'                { new_line lexbuf; docblock lexbuf }
  | "*/"                { token lexbuf }
  | eof                 { failwith "unterminated comment" }
  | _                   { raise (SyntaxError ("Lexer - Illegal character: " ^ Lexing.lexeme lexbuf)) }
