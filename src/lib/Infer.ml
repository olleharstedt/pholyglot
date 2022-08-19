(* Module to infer types of local variables *)

let infer_expression : (Ast.expression -> Ast.typ) = function
    | Num _ -> Int
    | Plus (_, _) -> Int
    | Minus (_, _) -> Int
    | Times (_, _) -> Int
    | Div (_, _) -> Int
    | Variable id -> failwith ("Can't infer type of variable " ^ id)

let infer_stmt : (Ast.statement -> Ast.statement) = function
    | Assignment (Infer_me, id, expr) -> Assignment (infer_expression expr, id, expr)
    | s -> s

let infer_declaration : (Ast.declaration -> Ast.declaration) = function
    (*
    | Function of function_name * param list * statement list * typ
    | Struct of struct_name * struct_field list
    *)
    | Function (name, params, stmts, typ) -> Function (name, params, List.map infer_stmt stmts, typ)
    | d -> d

let run : (Ast.program -> Ast.program) = function
    | Declaration_list decls -> Declaration_list (List.map infer_declaration decls)
