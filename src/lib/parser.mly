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

let get_class_properties elems =
    List.map (fun e -> match e with Property p -> p)
    (List.filter (fun e -> match e with Property _ -> true | _ -> false) elems)

let get_class_methods elems =
    List.map (fun e -> match e with Method m -> m)
    (List.filter (fun e -> match e with Method _ -> true | _ -> false) elems)
%}

%token <int> INT
%token <float> FLOAT
%token <string> NAME
%token <string> VAR_NAME  (* This one with dollar-sign *)
%token <string> STRING_LITERAL
%token <string> CLASS_NAME
(*%token <Docblockparser.token list> DOCBLOCK*)
%token <string> DOCBLOCK_AS_STR
%token START_SCRIPT "<?php // @pholyglot"
%token PLUS "+"
%token PLUSPLUS "++"
%token MINUS "-"
%token MINUSMINUS "--"
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
%token AMPERSAND "&"
%token INT_TYPE "int"
%token FLOAT_TYPE "float"
%token VOID_TYPE "void"
%token STRING_TYPE "string"
%token ARRAY_TYPE "array"
%token RETURN "return"
%token NEW "new"
%token FUNCTION "function"
%token CLASS "class"
%token PUBLIC "public"
%token PRIVATE "private"
%token FOREACH "foreach"
%token AS "as"

%left DOT
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */

%type <declaration> declaration
%type <statement> statement
%type <typ> typ
%type <expression> expr

/* changed the type, because the script does not return one value, but all
 * results which are calculated in the file */
%start<Ast.program> program
%%

program:
    | START_SCRIPT d=list(declaration); EOF {Declaration_list d}

declaration:
    | "function" name=NAME "(" params=separated_list(COMMA, arg_decl) ")" ":" t=typ "{" stmts=list(statement) "}" {
        Function {
            name;
            docblock = [];
            params;
            stmts;
            function_type = Function_type {return_type = t; arguments = get_arg_types_from_args params};
        }
    }
    | doc=DOCBLOCK_AS_STR "function" name=NAME "(" params=separated_list(COMMA, arg_decl) ")" ":" t=typ "{" stmts=list(statement) "}" {
        let linebuf = Lexing.from_string doc in
        let cb = Docblockparser.docblock Docblocklexer.docblock linebuf in
        Function {
            name;
            docblock = cb;
            params;
            stmts;
            function_type = Function_type {return_type = t; arguments = get_arg_types_from_args params};
        }
    }
    | "class" s=CLASS_NAME "{" elems=list(class_element) "}" {Class {name = s; kind = Infer_kind; properties = get_class_properties(elems); methods = get_class_methods(elems)}}

statement: 
  | "return" e=expr ";"                                      {Return e}
  | v=lvalue "=" e=expr ";"                                  {Assignment (Infer_me, v, e)}
  | v=lvalue "++" ";"                                        {Plusplus v}
  | v=lvalue "--" ";"                                        {Minusminus v}
  | n=NAME "(" args_list=separated_list(COMMA, expr) ")" ";" {Function_call (Infer_me, n, args_list)}
  | FOREACH "(" n=VAR_NAME AS m=VAR_NAME ")" "{" stmts=list(statement) "}" {Foreach {arr = Variable n; key = None; value = Variable m; value_typ = Infer_me; value_typ_constant = Nil; body = stmts} }

(* TODO: Must use property and method in same rule to avoid reduce/reduce ? *)
class_element: 
  | property_modifier t=typ s=VAR_NAME ";"  {Property ("__object_property_" ^ s, t)}
  | property_modifier {Property ("", Infer_me)}
  | property_modifier "function" name=NAME "(" params=separated_list(COMMA, arg_decl) ")" ":" t=typ "{" stmts=list(statement) "}" {
        Method {
            name;
            docblock = [];
            params;
            stmts;
            function_type = Function_type {return_type = t; arguments = get_arg_types_from_args params};
        }
    }

property_modifier:
  | "public" {}

(* TODO: Code duplication *)
method_:
    | method_modifier "function" name=NAME "(" params=separated_list(COMMA, arg_decl) ")" ":" t=typ "{" stmts=list(statement) "}" {
        Function {
            name;
            docblock = [];
            params;
            stmts;
            function_type = Function_type {return_type = t; arguments = get_arg_types_from_args params};
        }
    }

method_modifier:
  | "private" {}

arg_decl:
  | "array" "&" n=VAR_NAME    {RefParam (n, Fixed_array (Infer_me, None))}
  | t=typ "&" n=VAR_NAME      {raise (Parser_exception "Only array type can be passed by reference")}
  | "array" n=VAR_NAME        {raise (Parser_exception "Array must be passed as a reference - Pholly does not support array value semantics")}
  | t=typ n=VAR_NAME          {Param (n, t)}

typ:
  | "int"                       {Int : Ast.typ}
  | "float"                     {Float : Ast.typ}
  | "string"                    {String : Ast.typ}
  | "void"                      {Void : Ast.typ}
  | "array"                     {Fixed_array (Infer_me, None) : Ast.typ}
  | s=CLASS_NAME                {Class_type s : Ast.typ}
  (* TODO: User-defined type, class must start with upper-case letter *)
  | s=NAME                      {failwith ("Unknown type: " ^ s)}

lvalue:
  | n=VAR_NAME                  {Variable n}
  | id=NAME                     {Property_access ("__object_property_" ^ id)}
  | n=VAR_NAME "->" v=lvalue    {Object_access (n, v)}

expr:
  | MINUS i=INT                                                  {Num (-i)}
  | i=INT                                                        {Num i}
  | MINUS f=FLOAT                                                {Num_float (-. f)}
  | f=FLOAT                                                      {Num_float f}
  | s=STRING_LITERAL                                             {String s}
  | e=expr "+" f=expr                                            {Plus (e, f)} 
  | e=expr "-" f=expr                                            {Minus (e, f)} 
  | e=expr "*" f=expr                                            {Times (e, f)} 
  | e=expr "/" f=expr                                            {Div (e, f)} 
  | e=expr "." f=expr                                            {Concat (e, f)} 
  | e=expr "<" f=expr                                            {Lessthan (e, f)} 
  | e=expr ">" f=expr                                            {Greaterthan (e, f)} 
  | n=NAME "(" args_list=separated_list(COMMA, expr) ")"         {Function_call (Infer_me, n, args_list)}
  | n=VAR_NAME "[" e=expr "]"                                    {Array_access (n, e)}
  | e=expr "->" m=NAME "(" args_list=separated_list(COMMA, expr) ")" {Method_call {return_type = Infer_me; method_name = m; args = args_list; left_hand = e}}
  | e=expr "->" m=NAME                                               {Object_access (e, Property_access ("__object_property_" ^ m)) }
  | n=VAR_NAME                                                   {Variable n}
  | "new" s=CLASS_NAME "(" ")"                                   {New (Class_type s, [])}
  (* TODO "new" /** @alloc stack */ *)
  (*| "new" t=typ "{" struct_init=separated_list(COMMA, expr) "}"  {New (t, struct_init)}*)
  | "[" array_init=separated_list(COMMA, expr) "]"               {Array_init (Infer_me, None, array_init)}
