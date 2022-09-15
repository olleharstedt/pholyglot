exception Transpile_error of string

let kind_to_pholyglot k = match k with
    | Ast.Ref -> Pholyglot_ast.Ref
    | Ast.Val -> Pholyglot_ast.Val

(** Transpile from Pholly AST to Pholyglot AST *)
let rec typ_to_pholyglot t = match t with
    | Ast.Int -> Pholyglot_ast.Int
    | Ast.String -> Pholyglot_ast.String
    | Ast.String_literal -> Pholyglot_ast.String_literal
    | Ast.Fixed_array (t, n) -> Pholyglot_ast.Fixed_array ((typ_to_pholyglot t), n)
    | Ast.Void -> Pholyglot_ast.Void
    | Ast.Function_type {return_type; arguments} -> Pholyglot_ast.Function_type {return_type = typ_to_pholyglot return_type; arguments = List.map typ_to_pholyglot arguments}
    (** TODO: Should we infer types before moving to Pholyglot_ast? *)
    | Ast.Infer_me -> failwith "Infer before transpiling"
    | Ast.Class_type (n) -> Pholyglot_ast.Class_type (n)
    | t -> raise (Transpile_error ("typ_to_pholyglot: " ^ Ast.show_typ t))

let param_to_pholyglot (p: Ast.param) : Pholyglot_ast.param = match p with
    | Param (id, typ) -> Pholyglot_ast.Param (id, typ_to_pholyglot typ)

let rec lvalue_to_pholyglot lvalue = match lvalue with
    | Ast.Variable identifier -> Pholyglot_ast.Variable identifier
    | Ast.Property_access class_property_name -> Pholyglot_ast.Property_access class_property_name
    | Ast.Object_access (identifier, lvalue) -> Pholyglot_ast.Object_access (identifier, lvalue_to_pholyglot lvalue)

let rec expression_to_pholyglot exp = match exp with
    | Ast.String s -> Pholyglot_ast.String s
    | Ast.Num i -> Pholyglot_ast.Num i
    | Ast.Plus (i, j) -> Pholyglot_ast.Plus (expression_to_pholyglot i, expression_to_pholyglot j)
    | Ast.Minus (i, j) -> Pholyglot_ast.Minus (expression_to_pholyglot i, expression_to_pholyglot j)
    | Ast.Times (i, j) -> Pholyglot_ast.Times (expression_to_pholyglot i, expression_to_pholyglot j)
    | Ast.Div (i, j) -> Pholyglot_ast.Div (expression_to_pholyglot i, expression_to_pholyglot j)
    | Ast.Concat (s, t) -> Pholyglot_ast.Concat (expression_to_pholyglot s, expression_to_pholyglot t)
    | Ast.Variable id -> Pholyglot_ast.Variable id
    | Ast.Array_init (exprs) -> Pholyglot_ast.Array_init (List.map expression_to_pholyglot exprs)
    | Ast.Array_access (id, expr) -> Pholyglot_ast.Array_access (id, expression_to_pholyglot expr)
    | Ast.Function_call (typ, id, exprs) -> Pholyglot_ast.Function_call (typ_to_pholyglot typ, id, List.map expression_to_pholyglot exprs)
    | Ast.Coerce (t, e) -> Pholyglot_ast.Coerce (typ_to_pholyglot t, expression_to_pholyglot e)
    | Ast.Object_access (identifier, lvalue) -> Pholyglot_ast.Object_access (identifier, lvalue_to_pholyglot lvalue)
    | Ast.New (t, exprs) -> Pholyglot_ast.New (typ_to_pholyglot t, List.map expression_to_pholyglot exprs)
    | e -> failwith ("expression_to_pholyglot: " ^ (Ast.show_expression e))

let rec lvalue_to_pholyglot (l : Ast.lvalue) : Pholyglot_ast.lvalue = match l with
    | Ast.Variable id -> Pholyglot_ast.Variable id
    | Ast.Property_access class_property_name -> Pholyglot_ast.Property_access class_property_name
    | Ast.Object_access (identifier,  lvalue) -> Pholyglot_ast.Object_access (identifier, lvalue_to_pholyglot lvalue)

let statement_to_pholyglot s = match s with
    | Ast.Return exp -> Pholyglot_ast.Return (expression_to_pholyglot exp)
    | Ast.Assignment (typ, lvalue, expr) -> Pholyglot_ast.Assignment (typ_to_pholyglot typ, lvalue_to_pholyglot lvalue, expression_to_pholyglot expr)
    | Ast.Function_call (typ, identifier, exprs) -> Pholyglot_ast.Function_call (typ_to_pholyglot typ, identifier, List.map expression_to_pholyglot exprs)

let prop_to_pholyglot p : Pholyglot_ast.class_property = match p with
    | (name, t) -> (name, typ_to_pholyglot t)

let declaration_to_pholyglot (d : Ast.declaration) : Pholyglot_ast.declaration = match d with
    | Function (name, params, statements, typ) ->
        Pholyglot_ast.Function (
            name,
            List.map param_to_pholyglot params,
            List.map statement_to_pholyglot statements,
            typ_to_pholyglot typ
        )
    | Class (name, k, props) -> Pholyglot_ast.Class (name, kind_to_pholyglot k, List.map prop_to_pholyglot props)

(** Transpile from Pholly AST to Pholyglot AST *)
let run (ast : Ast.program) : Pholyglot_ast.program = match ast with
| Declaration_list ds -> 
    let declares : (Pholyglot_ast.declaration list) = List.map (fun d -> declaration_to_pholyglot d) ds in
    let open Pholyglot_ast in
    (
        Start_line,
        (* Include list *)
        [
            Include "stdio.h";
            Include "glib.h"
        ],
        (* Define list *)
        [
            Define ("$", None);
            Define ("class", Some "struct");
            Define ("__PHP__", Some "0");
            (* TODO: new_stack and new_heap? new_pool? *)
            Define ("new_(x)", Some "alloca(sizeof(struct x))");
        ],
        (* Stubs *)
        [
            "class GString { public $str; public function __construct($str) { $this->str = $str; } }\n";
            "function g_string_new(string $str) { return new GString($str); }\n";
            "function g_string_append(GString $s1, string $s2) { return new GString($s1->str . $s2); }\n";
            "function new_($class) { return new $class; }\n";
        ],
        (* Declarations *)
        declares,
        End_line
    )
