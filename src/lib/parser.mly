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
%token <token list> DOCBLOCK
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
%token AMPERSAND "&"
%token INT_TYPE "int"
%token VOID_TYPE "void"
%token STRING_TYPE "string"
%token ARRAY_TYPE "array"
%token RETURN "return"
%token NEW "new"
%token FUNCTION "function"
%token CLASS "class"
%token PUBLIC "public"
%token DOCBLOCK_PARAM "@param"

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
    | START_SCRIPT d=list(declaration); EOF {Declaration_list d}

declaration:
    | docblock_lines=list(dockblock_line) "function" name=NAME "(" params=separated_list(COMMA, arg_decl) ")" ":" t=typ "{" stmts=list(statement) "}" {
        Function {
            name;
            docblock = docblock_lines;
            params;
            stmts;
            function_type = Function_type {return_type = t; arguments = get_arg_types_from_args params};
        }
    }
    | "class" s=CLASS_NAME "{" f=list(class_property) "}" {Class (s, Infer_kind, f)}

statement: 
  | "return" e=expr ";"                                      {Return e}
  | v=lvalue "=" e=expr ";"                                  {Assignment (Infer_me, v, e)}
  | n=NAME "(" args_list=separated_list(COMMA, expr) ")" ";" {Function_call (Infer_me, n, args_list)}

dockblock_line:
  | "@param"  {Param ("moo", Int) : Ast.docblock_comment}

class_property: "public" t=typ "$" s=NAME ";"  {("__object_property_" ^ s, t)}

arg_decl:
  | "array" "&" "$" n=NAME    {RefParam (n, Fixed_array (Infer_me, None))}
  | t=typ "&" "$" n=NAME      {raise (Parser_exception "Only array type can be passed by reference")}
  | "array" "$" n=NAME        {raise (Parser_exception "Array must be passed as a reference - Pholly does not support array value semantics")}
  | t=typ "$" n=NAME          {Param (n, t)}

typ:
  | "int"                       {Int : Ast.typ}
  | "string"                    {String : Ast.typ}
  | "void"                      {Void : Ast.typ}
  | "array"                     {Fixed_array (Infer_me, None) : Ast.typ}
  | s=CLASS_NAME                {Class_type s : Ast.typ}
  (* TODO: User-defined type, class must start with upper-case letter *)
  | s=NAME                      {failwith ("Unknown type: " ^ s)}

lvalue:
  | "$" n=NAME                  {Variable n}
  | id=NAME                     {Property_access ("__object_property_" ^ id)}
  | "$" n=NAME "->" v=lvalue    {Object_access (n, v)}

expr:
  | i=INT                                                        {Num i}
  | s=STRING_LITERAL                                             {String s}
  | e=expr "+" f=expr                                            {Plus (e, f)} 
  | e=expr "-" f=expr                                            {Minus (e, f)} 
  | e=expr "*" f=expr                                            {Times (e, f)} 
  | e=expr "/" f=expr                                            {Div (e, f)} 
  | e=expr "." f=expr                                            {Concat (e, f)} 
  | n=NAME "(" args_list=separated_list(COMMA, expr) ")"         {Function_call (Infer_me, n, args_list)}
  | "$" n=NAME "[" e=expr "]"                                    {Array_access (n, e)}

  | "$" n=NAME "->" e=expr                                       {Object_access (n, e)}
  | n=NAME                                                       {Property_access ("__object_property_" ^ n)}

  | "$" n=NAME                                                   {Variable n}
  | "new" s=CLASS_NAME "(" ")"                                   {New (Class_type s, [])}
  (*| "new" t=typ "{" struct_init=separated_list(COMMA, expr) "}"  {New (t, struct_init)}*)
  | "[" array_init=separated_list(COMMA, expr) "]"               {Array_init array_init}
