%{
  open Ast
%}

%token ARRAY_TYPE "array"
%token DOCBLOCK_PARAM "@param"
%token INT_TYPE "int"
%token FLOAT_TYPE "float"
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
  | "@param" t=typ s=VAR_NAME                 {DocParam (s, t) }
  | "@param" "array" s=VAR_NAME               { failwith "Must be more precise in docblock than just 'array'" }
  | "@param" "array" "<" t=typ ">" s=VAR_NAME {DocParam (s, Dynamic_array t) }

typ:
  | "int"      {Int : Ast.typ}
  | "float"    {Float : Ast.typ}
  | "string"   {String : Ast.typ}
  | s=CLASS_NAME {Class_type (s, Infer_allocation_strategy) : Ast.typ}
