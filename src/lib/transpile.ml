(** Transpile from Pholly AST to Pholyglot AST *)
let rec typ_to_pholyglot t = match t with
    | Ast.Int -> Pholyglot_ast.Int
    | Ast.String -> Pholyglot_ast.String
    | Ast.Fixed_array t -> Pholyglot_ast.Fixed_array (typ_to_pholyglot t)
    | Ast.Void -> Pholyglot_ast.Void
    | Ast.Var_args -> Pholyglot_ast.Var_args
    | Ast.Function_type (t, ts) -> Pholyglot_ast.Function_type (typ_to_pholyglot t, List.map typ_to_pholyglot ts)
    (** TODO: Should we infer types before moving to Pholyglot_ast? *)
    | Ast.Infer_me -> failwith "Infer before transpiling"
    | t -> failwith (Ast.show_typ t)

let param_to_pholyglot (p: Ast.param) : Pholyglot_ast.param = match p with
    | Param (id, typ) -> Pholyglot_ast.Param (id, typ_to_pholyglot typ)

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

let statement_to_pholyglot s = match s with
    | Ast.Return exp -> Pholyglot_ast.Return (expression_to_pholyglot exp)
    | Ast.Assignment (typ, id, expr) -> Pholyglot_ast.Assignment (typ_to_pholyglot typ, id, expression_to_pholyglot expr)
    | Ast.Function_call (typ, identifier, exprs) -> Pholyglot_ast.Function_call (typ_to_pholyglot typ, identifier, List.map expression_to_pholyglot exprs)

let declaration_to_pholyglot (d : Ast.declaration) : Pholyglot_ast.declaration = match d with
    | Function (name, params, statements, typ) ->
        Pholyglot_ast.Function (
            name,
            List.map param_to_pholyglot params,
            List.map statement_to_pholyglot statements,
            typ_to_pholyglot typ
        )
    | _ -> failwith "declaration_to_pholyglot"

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
            Define ("function", None);
            Define ("__PHP__", Some "0")
        ],
        (* Stubs *)
        [
            "class GString { public $str; public function __construct($str) { $this->str = $str; } }\n";
            "function g_string_new(string $str) { return new GString($str); }\n";
            "function g_string_append(GString $s1, string $s2) { return new GString($s1->str . $s2); }\n";
        ],
        (* Declarations *)
        declares,
        End_line
    )
