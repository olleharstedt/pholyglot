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
}

(** Copied from Jacques-Henri Jourdan, Inria Paris *)
let digit = ['0'-'9']
let nondigit = ['_' 'a'-'z' 'A'-'Z']
let nonzero_digit = ['1'-'9']
let decimal_constant = nonzero_digit digit*
let identifier = nondigit (nondigit|digit)*
let whitespace_char_no_newline = [' ' '\t' '\012' '\r']
let integer_constant = decimal_constant

rule token = parse
  | whitespace_char_no_newline+   { token lexbuf }
  | '\n'                          { new_line lexbuf; initial_linebegin lexbuf }
  | integer_constant as i         { INT (int_of_string i) }
  | "0"                           { INT 0 }
  | "="                           { EQ }
  | "=="                          { EQEQ }
  | "<"                           { LT }
  | ">"                           { GT }
  | ';'                           { SEMICOLON }
  | ':'                           { COLON }
  | '+'                           { PLUS }
  | '-'                           { MINUS }
  | "{"                           { LBRACE }
  | "}"                           { RBRACE }
  | "["                           { LBRACK }
  | "]"                           { RBRACK }
  | '('                           { LPAREN }
  | ')'                           { RPAREN }
  | ','                           { COMMA }
  | "return"                      { RETURN }
  | "new"                         { NEW }
  | "let"                         { LET }
  | "local"                       { LOCAL }
  | "region"                      { REGION }
  | "with"                        { WITH }
  | "struct"                      { STRUCT }
  | "function"                    { FUNCTION }
  | "~"                           { TILDE }
  | "@"                           { AT }
  | "function"                    { FUNCTION }
  | "in"                          { IN }
  | identifier as id              { NAME id }
  | eof                           { EOF }
  | _ { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }

and initial_linebegin = parse
  | '\n'                          { new_line lexbuf; initial_linebegin lexbuf }
  | whitespace_char_no_newline    { initial_linebegin lexbuf }
  | ""                            { token lexbuf }
