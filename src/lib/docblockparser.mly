%{
  open Ast
%}

%token ARRAY_TYPE "array"
%token DOCBLOCK_PARAM "@param"
%token INT_TYPE "int"
%token STRING_TYPE "string"
%token LT "<"
%token GT ">"
%token COMMA ","
%token <string> NAME
%token <string> VAR_NAME
%token <string> CLASS_NAME
%token START_OF_COMMENT "/**"
%token END_OF_COMMENT "*/"
%token EOF

%start<Ast.docblock_comment list> docblock
%%

docblock:
  | d=list(docblock_line) EOF {d}

docblock_line:
  (*| DOC_DOCBLOCK_PARAM DOC_INT_TYPE DOC_DOLLAR n=DOC_NAME {Param (n, Int) : Ast.docblock_comment }*)
  | "@param" {DocParam ("asd", Int) }
  | "@param" t=typ s=VAR_NAME {DocParam (s, t) }
  | "@param" "array" "<" t=typ ">" s=VAR_NAME {DocParam (s, Dynamic_array t) }
  | START_OF_COMMENT { failwith "wrong" }

typ:
  | "int"      {Int : Ast.typ}
  | "string"   {String : Ast.typ}
  | s=CLASS_NAME {Class_type s : Ast.typ}
