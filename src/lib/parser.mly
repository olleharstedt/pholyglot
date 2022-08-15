/**************************************************************************/
/*                                                                        */
/*  Menhir                                                                */
/*                                                                        */
/*  François Pottier, INRIA Rocquencourt                                  */
/*  Yann Régis-Gianas, PPS, Université Paris Diderot                      */
/*                                                                        */
/*  Copyright 2005-2008 Institut National de Recherche en Informatique    */
/*  et en Automatique. All rights reserved. This file is distributed      */
/*  under the terms of the Q Public License version 1.0, with the change  */
/*  described in file LICENSE.                                            */
/*                                                                        */
/**************************************************************************/

%{
  open Ast
%}

%token <int> INT
%token <string> NAME
%token CONSTANT STRING_LITERAL
%token PLUS "+"
%token MINUS "-"
(*%token TIMES DIV*)
%token COLON ":"
%token SEMICOLON ";"
%token COMMA ","
%token EOF
%token EQEQ "=="
%token EQ "="
%token LT "<"
%token GT ">"
%token LPAREN "("
%token RPAREN ")"
%token LBRACE "{"
%token RBRACE "}"
%token LBRACK "["
%token RBRACK "]"
%token TILDE "~"
%token AT "@"
%token RETURN "return"
%token NEW "new"
%token LET "let"
%token LOCAL "local"
%token REGION "region"
%token WITH "with"
%token STRUCT "struct"
%token FUNCTION "function"
%token IN "in"

%left PLUS MINUS        /* lowest precedence */
(*%left TIMES DIV         /* medium precedence */*)
%nonassoc UMINUS        /* highest precedence */

%type <declaration> declaration
%type <statement> statement
%type <struct_field> struct_field
%type <typ> typ
%type <expression> expr

/* changed the type, because the script does not return one value, but all
 * results which are calculated in the file */
%start<Ast.program> program
%%

program:
    | d=list(declaration); EOF {Declaration_list d}

(*NAME int NAME main LPAREN RPAREN LBRACE RETURN INT0 SEMICOLON RBRACE*)
(*int main() { return 0; }*)
(*declaration: t=NAME n=NAME LPAREN RPAREN LBRACE RBRACE {Function (n, [], [], Int)}*)
declaration:
    | "function" f=NAME "(" ")" ":" t=typ "{" s=list(statement) "}" {Function (f, [], s, t)}
    | "struct" s=NAME "=" "{" f=list(struct_field) "}" {Struct (s, f)}

statement: 
  | "return" n=INT ";"          {Return (Num n)}
  | "let" v=NAME "=" e=expr ";" {Assignment (Infer_me, v, e)}
  | "new" "region" r=NAME ";"   {New_region r}

struct_field: t=typ s=NAME ";"  {(s, t)}

typ:
  | n=NAME                      {type_of_string n Nonlocal }
  | TILDE n=NAME                {type_of_string n Local }
  | AT n=NAME                   {type_of_string n (Regional None)}

expr:
  | i=INT                                                                   {Num i}
  | e=expr "+" f=expr                                                       {Plus (e, f)} 
  | "new" t=typ "{" struct_init=separated_list(COMMA, expr) "}"             {New (t, struct_init)}
  | "new" t=typ "{" struct_init=separated_list(COMMA, expr) "}" "in" r=NAME {New (add_region_to_typ t r, struct_init)}

(* let p = new Point {1, 2}; *)
