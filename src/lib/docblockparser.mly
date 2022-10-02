%{
  open Ast
%}

%token ARRAY_TYPE "array"
%token DOLLAR "$"
%token DOCBLOCK_PARAM "@param"
%token INT_TYPE "int"
%token LT "<"
%token GT ">"
%token COMMA ","
%token <string> NAME
%token END_OF_COMMENT "*/"
%token EOF

%start<Ast.docblock_comment list> docblock
%%

docblock:
  | d=list(docblock_line) {d}

docblock_line:
  (*| DOC_DOCBLOCK_PARAM DOC_INT_TYPE DOC_DOLLAR n=DOC_NAME {Param (n, Int) : Ast.docblock_comment }*)
  | DOCBLOCK_PARAM {Param ("asd", Int) : Ast.docblock_comment}
