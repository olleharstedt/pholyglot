(** Transpile from Pholly AST to Pholyglot AST *)
let typ_to_pholyglot t = match t with
    | Ast.Int -> Pholyglot_ast.Int
    | _ -> failwith "typ_to_pholyglot"

let param_to_pholyglot (p: Ast.param) : Pholyglot_ast.param = match p with
    | Param (id, typ) -> Pholyglot_ast.Param (id, typ_to_pholyglot typ)

let expression_to_pholyglot exp = match exp with
    | Ast.Num i -> Pholyglot_ast.Num i
    | _ -> failwith "expression_to_pholyglot"

let statement_to_pholyglot s = match s with
    | Ast.Return exp -> Pholyglot_ast.Return (expression_to_pholyglot exp)
    | _ -> failwith "statement_to_pholyglot"

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
        [],
        (* Define list *)
        [
            Define ("function", None)
        ],
        (* Declarations *)
        declares,
        End_line
    )
