%{
  open Ast
%}

%token ARRAY_TYPE "array"
%token LIST_TYPE "SplDoublyLinkedList"
%token DOCBLOCK_PARAM "@param"
%token DOCBLOCK_ALLOC "@alloc"
%token DOCBLOCK_VAR "@var"
%token INT_TYPE "int"
%token FLOAT_TYPE "float"
%token STRING_TYPE "string"
%token ALLOC_BOEHM "boehm"
%token ALLOC_ARENA "arena"
%token ALLOC_STACK "stack"
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
  (* TODO: Duplication *)
  | "@param" t=typ s=VAR_NAME                  {DocParam (s, t) }
  | "@param" "array" s=VAR_NAME                { failwith "Must be more precise in docblock than just 'array'" }
  | "@param" "array" "<" t=typ ">" s=VAR_NAME  {DocParam (s, Dynamic_array t) }
  | "@param" "SplDoublyLinkedList" "<" t=typ ">" s=VAR_NAME {DocParam (s, List t) }
  | "@alloc" at=alloc_typ                      {DocAlloc at}
  | "@var" t=typ                               {DocVar (None, t)}
  | "@var" "array" s=VAR_NAME                  { failwith "Must be more precise in docblock than just 'array'" }
  | "@var" "array" "<" t=typ ">"               {DocVar (None, Dynamic_array t) }
  | "@var" "SplDoublyLinkedList" "<" t=typ ">" {DocVar (None, List t) }

typ:
  | "int"      {Int : Ast.typ}
  | "float"    {Float : Ast.typ}
  | "string"   {String : Ast.typ}
  | s=CLASS_NAME {Class_type (s, Infer_allocation_strategy) : Ast.typ}

alloc_typ:
  | "boehm"     {Boehm}
  | "arena"     {Arena}
  | "stack"     {Stack}
  | s=NAME      {raise (Ast.DocblockParseError ("Invalid alloc type: " ^ s))}
  (* TODO: Catch all strings except *\ *)
