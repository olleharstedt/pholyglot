%{
  open Ast
%}

%token ARRAY_TYPE "array"
%token DOLLAR "$"
%token DOCBLOCK_PARAM "@param"
%token INT_TYPE "int"
%token STRING_TYPE "string"
%token LT "<"
%token GT ">"
%token COMMA ","
%token <string> NAME
%token START_OF_COMMENT "/**"
%token END_OF_COMMENT "*/"
%token EOF

%start<Ast.docblock_comment list> docblock
%%

docblock:
  | d=list(docblock_line) EOF {d}

docblock_line:
  (*| DOC_DOCBLOCK_PARAM DOC_INT_TYPE DOC_DOLLAR n=DOC_NAME {Param (n, Int) : Ast.docblock_comment }*)
  | DOCBLOCK_PARAM {DocParam ("asd", Int) }
  | DOCBLOCK_PARAM t=typ DOLLAR s=NAME {DocParam (s, t) }
  | DOCBLOCK_PARAM "array" "<" t=typ ">" DOLLAR s=NAME {DocParam (s, Dynamic_array t) }
  | START_OF_COMMENT { failwith "wrong" }

typ:
  | "int"    {Int : Ast.typ}
  | "string" {String : Ast.typ}
