%{
  open Ast
%}

%token DOC_ARRAY_TYPE "array"
%token DOC_DOLLAR "$"
%token DOC_DOCBLOCK_PARAM "@param"
%token DOC_INT_TYPE "int"
%token DOC_LT "<"
%token DOC_GT ">"
%token DOC_COMMA ","
%token <string> DOC_NAME

%start<Ast.docblock_comment list> docblock
%%

docblock:
  | d=list(docblock_line) {d}

docblock_line:
  | DOC_DOCBLOCK_PARAM DOC_INT_TYPE n=DOC_NAME {Param (n, Int) : Ast.docblock_comment }
