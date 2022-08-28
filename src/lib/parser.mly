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
%token <string> STRING_LITERAL
%token <string> CLASS_NAME
%token CONSTANT
%token START_SCRIPT "<?php // @pholyglot"
%token PLUS "+"
%token MINUS "-"
%token TIMES "*"
%token DIV "/"
%token COLON ":"
%token SEMICOLON ";"
%token COMMA ","
%token DOT "."
%token QUOTE "\""
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
%token ARROW "->"
%token DOLLAR "$"
%token INT_TYPE "int"
%token STRING_TYPE "string"
%token RETURN "return"
%token NEW "new"
%token FUNCTION "function"
%token CLASS "class"
%token PUBLIC "public"

%left DOT
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */

%type <declaration> declaration
%type <statement> statement
%type <class_property> class_property
%type <typ> typ
%type <expression> expr

/* changed the type, because the script does not return one value, but all
 * results which are calculated in the file */
%start<Ast.program> program
%%

program:
    | s=START_SCRIPT d=list(declaration); EOF {Declaration_list d}

(*NAME int NAME main LPAREN RPAREN LBRACE RETURN INT0 SEMICOLON RBRACE*)
(*int main() { return 0; }*)
(*declaration: t=NAME n=NAME LPAREN RPAREN LBRACE RBRACE {Function (n, [], [], Int)}*)
declaration:
    | "function" f=NAME "(" ")" ":" t=typ "{" s=list(statement) "}" {Function (f, [], s, t)}
    | "class" s=CLASS_NAME "{" f=list(class_property) "}" {Class (s, f)}

statement: 
  | "return" e=expr ";"                                      {Return e}
  | "$" n=lvalue "=" e=expr ";"                                {Assignment (Infer_me, n, e)}
  | n=NAME "(" args_list=separated_list(COMMA, expr) ")" ";" {Function_call (Infer_me, n, args_list)}

class_property: "public" t=typ "$" s=NAME ";"  {(s, t)}

typ:
  | t=INT_TYPE              {Int : Ast.typ}
  | "string"                {String : Ast.typ}
  (* TODO: User-defined type, class must start with upper-case letter *)
  | s=NAME                  {failwith ("Unknown type: " ^ s)}

lvalue:
  | id=NAME                 {Identifier id}
  | n=NAME "->" v=lvalue    {Object_access (n, v)}

expr:
  | i=INT                                                        {Num i}
  | s=STRING_LITERAL                                             {String s}
  | n=NAME                                                       {Identifier n}
  | e=expr "+" f=expr                                            {Plus (e, f)} 
  | e=expr "-" f=expr                                            {Minus (e, f)} 
  | e=expr "*" f=expr                                            {Times (e, f)} 
  | e=expr "/" f=expr                                            {Div (e, f)} 
  | e=expr "." f=expr                                            {Concat (e, f)} 
  | n=NAME "(" args_list=separated_list(COMMA, expr) ")"         {Function_call (Infer_me, n, args_list)}
  | "$" n=NAME "[" e=expr "]"                                    {Array_access (n, e)}
  | "$" n=NAME "->" e=expr                                       {Object_access (n, e)}
  | "$" n=NAME                                                   {Variable n}
  | "new" s=CLASS_NAME "(" ")"                                   {New (Class_type s, [])}
  (*| "new" t=typ "{" struct_init=separated_list(COMMA, expr) "}"  {New (t, struct_init)}*)
  | "[" array_init=separated_list(COMMA, expr) "]"                {Array_init array_init}
