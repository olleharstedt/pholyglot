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
  | "return"                      { RETURN }
  | "new"                         { NEW }
  | "function"                    { FUNCTION }
  | "class"                       { CLASS }
  | "public"                      { PUBLIC }
  | "int"                         { INT_TYPE }
  | "string"                      { STRING_TYPE }
  | "$"                           { DOLLAR }
  (*| '"'                           { read_string (Buffer.create 17) lexbuf }*)
  | ['"'] [^ '"']+ ['"'] as s     { STRING_LITERAL s }
  | identifier as id              { NAME id }
  | eof                           { EOF }
  | _ { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }

and initial_linebegin = parse
  | '\n'                          { new_line lexbuf; initial_linebegin lexbuf }
  | whitespace_char_no_newline    { initial_linebegin lexbuf }
  | ""                            { token lexbuf }

  (*
and read_string buf = parse
    | '"'       { STRING_LITERAL (Buffer.contents buf) }
    | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
    | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
    | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
    | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
    | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
    | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
    | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
    | [^ '"' '\\']+
        { Buffer.add_string buf (Lexing.lexeme lexbuf);
        read_string buf lexbuf
    }
    | _ { raise (Error ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
    | eof { raise (Error ("String is not terminated")) }
    *)
